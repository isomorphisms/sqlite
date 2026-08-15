module SQLite.BTreeRead

import Data.List
import Data.Nat
import SQLite.File
import SQLite.Page
import SQLite.Varint

%default covering

||| An undecoded SQLite table record.  `payload` begins with the record-format
||| header-size varint; interpreting serial types belongs to a separate layer.
public export
record RawTableRecord where
  constructor MkRawTableRecord
  leafPage : Nat
  rowId : Integer
  payload : List Integer

public export
Eq RawTableRecord where
  left == right =
    left.leafPage == right.leafPage
      && left.rowId == right.rowId
      && left.payload == right.payload

public export
Show RawTableRecord where
  show item =
    "rowid " ++ show item.rowId ++ ", "
      ++ show (length item.payload) ++ " payload bytes (page "
      ++ show item.leafPage ++ ")"

byteAt : String → Nat → List Integer → Either String Integer
byteAt context offset bytes =
  case getAt offset bytes of
    Nothing ⇒ Left (context ++ " ends before byte " ++ show offset)
    Just byte ⇒
      if byte >= 0 && byte <= 255
        then Right byte
        else Left (context ++ " contains a non-byte value")

big16At : String → Nat → List Integer → Either String Integer
big16At context offset bytes = do
  high <- byteAt context offset bytes
  low <- byteAt context (offset + 1) bytes
  pure (high * 256 + low)

big32At : String → Nat → List Integer → Either String Integer
big32At context offset bytes = do
  first <- byteAt context offset bytes
  second <- byteAt context (offset + 1) bytes
  third <- byteAt context (offset + 2) bytes
  fourth <- byteAt context (offset + 3) bytes
  pure (first * 16777216 + second * 65536 + third * 256 + fourth)

exactSlice : String → Nat → Nat → List Integer → Either String (List Integer)
exactSlice context offset amount bytes =
  let answer = take amount (drop offset bytes)
   in if length answer == amount
        then Right answer
        else Left (context ++ " is truncated")

boundedNat : String → Integer → Nat → Either String Nat
boundedNat label value upper =
  if value < 0
    then Left (label ++ " is negative")
    else if value > natToInteger upper
      then Left (label ++ " exceeds its safe parser bound")
      else Right (integerToNat value)

signed64 : Integer → Integer
signed64 unsigned =
  if unsigned >= 9223372036854775808
    then unsigned - 18446744073709551616
    else unsigned

varintAt : String → Nat → Nat → List Integer → Either String DecodedVarint
varintAt context limit offset bytes =
  if offset >= limit
    then Left (context ++ " starts outside usable page bytes")
    else
      case decode (take (minus limit offset) (drop offset bytes)) of
        Left error ⇒ Left (context ++ ": " ++ error)
        Right decoded ⇒ Right decoded

validPageNumber : DatabaseHeader → String → Integer → Either String Nat
validPageNumber header label number =
  if number <= 0 || number > natToInteger header.pageCount
    then Left
      (label ++ " page " ++ show number ++ " is outside database bounds 1.."
        ++ show header.pageCount)
    else Right (integerToNat number)

headerOffset : Nat → Nat
headerOffset 1 = 100
headerOffset _ = 0

headerLength : PageKind → Nat
headerLength IndexInterior = 12
headerLength TableInterior = 12
headerLength IndexLeaf = 8
headerLength TableLeaf = 8

distinct : Eq item ⇒ List item → Bool
distinct [] = True
distinct (item :: rest) = not (elem item rest) && distinct rest

cellPointers : SQLiteFile → Nat → PageHeader → List Integer → Either String (List Nat)
cellPointers database pageNumber header page = do
  count <- boundedNat "B-tree cell count" header.cellCount database.header.usableSize
  let pointerStart = headerOffset pageNumber + headerLength header.kind
  if pointerStart + 2 * count <= database.header.usableSize
    then Right ()
    else Left ("cell pointer array overruns usable bytes on page " ++ show pageNumber)
  if header.cellContentStart >= natToInteger (pointerStart + 2 * count)
       && header.cellContentStart <= natToInteger database.header.usableSize
    then Right ()
    else Left ("invalid cell-content boundary on page " ++ show pageNumber)
  pointers <- readPointers count pointerStart
  if distinct pointers
    then Right pointers
    else Left ("duplicate cell pointer on page " ++ show pageNumber)
  where
    readPointers : Nat → Nat → Either String (List Nat)
    readPointers Z _ = Right []
    readPointers (S remaining) offset = do
      raw <- big16At ("cell pointer array on page " ++ show pageNumber) offset page
      pointer <- boundedNat "cell offset" raw database.header.usableSize
      if raw >= header.cellContentStart
           && pointer < database.header.usableSize
        then Right ()
        else Left ("cell points outside usable page " ++ show pageNumber)
      later <- readPointers remaining (offset + 2)
      pure (pointer :: later)

localPayloadBytes : Nat → Integer → Either String Nat
localPayloadBytes usable payloadSize =
  let u = natToInteger usable
      maximumLocal = u - 35
      minimumLocal = ((u - 12) * 32 `div` 255) - 23
      candidate = minimumLocal + ((payloadSize - minimumLocal) `mod` (u - 4))
      local =
        if payloadSize <= maximumLocal
          then payloadSize
          else if candidate <= maximumLocal then candidate else minimumLocal
   in if local >= 0 && local <= payloadSize
        then Right (integerToNat local)
        else Left "invalid calculated local payload size"

readOverflow : SQLiteFile → Nat → List Nat → Nat → Nat →
               Either String (List Nat, List Integer)
readOverflow database Z seen _ _ = Left "overflow chain exceeds database page count"
readOverflow database (S fuel) seen pageNumber remaining = do
  if pageNumber == 0
    then Left "overflow chain ends before the record payload is complete"
    else Right ()
  if elem pageNumber seen
    then Left ("cycle or shared page at overflow page " ++ show pageNumber)
    else Right ()
  page <- pageBytes database pageNumber
  nextRaw <- big32At ("overflow page " ++ show pageNumber) 0 page
  let capacity = minus database.header.usableSize 4
  let amount = min remaining capacity
  chunk <- exactSlice ("overflow page " ++ show pageNumber) 4 amount page
  let nowSeen = pageNumber :: seen
  if remaining <= capacity
    then
      if nextRaw == 0
        then Right (nowSeen, chunk)
        else Left ("final overflow page " ++ show pageNumber ++ " has a nonzero successor")
    else do
      next <- validPageNumber database.header "overflow" nextRaw
      (laterSeen, later) <- readOverflow database fuel nowSeen next (minus remaining capacity)
      pure (laterSeen, chunk ++ later)

readLeafCell : SQLiteFile → Nat → List Integer → List Nat → Nat →
               Either String (List Nat, RawTableRecord)
readLeafCell database pageNumber page seen cellOffset = do
  let usable = database.header.usableSize
  payloadVarint <- varintAt "table-leaf payload length" usable cellOffset page
  payloadSize <- boundedNat "record payload length" payloadVarint.value (length database.contents)
  let rowOffset = cellOffset + payloadVarint.bytesRead
  rowVarint <- varintAt "table-leaf rowid" usable rowOffset page
  let payloadOffset = rowOffset + rowVarint.bytesRead
  localAmount <- localPayloadBytes usable payloadVarint.value
  if payloadOffset + localAmount <= usable
    then Right ()
    else Left ("local payload overruns leaf page " ++ show pageNumber)
  local <- exactSlice ("local payload on leaf page " ++ show pageNumber)
                      payloadOffset localAmount page
  if payloadSize == localAmount
    then Right (seen, MkRawTableRecord pageNumber (signed64 rowVarint.value) local)
    else do
      if payloadOffset + localAmount + 4 <= usable
        then Right ()
        else Left ("overflow pointer overruns leaf page " ++ show pageNumber)
      firstRaw <- big32At ("overflow pointer on leaf page " ++ show pageNumber)
                          (payloadOffset + localAmount) page
      first <- validPageNumber database.header "overflow" firstRaw
      (nowSeen, overflow) <- readOverflow database database.header.pageCount seen first
                                             (minus payloadSize localAmount)
      pure (nowSeen, MkRawTableRecord pageNumber (signed64 rowVarint.value)
                                             (local ++ overflow))

readLeafCells : SQLiteFile → Nat → List Integer → List Nat → List Nat →
                Either String (List Nat, List RawTableRecord)
readLeafCells database pageNumber page seen [] = Right (seen, [])
readLeafCells database pageNumber page seen (pointer :: rest) = do
  (nowSeen, item) <- readLeafCell database pageNumber page seen pointer
  (finalSeen, later) <- readLeafCells database pageNumber page nowSeen rest
  pure (finalSeen, item :: later)

interiorChildren : SQLiteFile → Nat → PageHeader → List Integer → List Nat →
                   Either String (List (Nat, Maybe Integer))
interiorChildren database pageNumber header page pointers = do
  keyed <- readCells pointers
  if strictlyIncreasing (map snd keyed)
    then Right ()
    else Left ("table-interior keys are not strictly increasing on page "
                ++ show pageNumber)
  rightRaw <-
    case header.rightmostChild of
      Nothing ⇒ Left ("table-interior page " ++ show pageNumber ++ " has no right child")
      Just child ⇒ Right child
  right <- validPageNumber database.header "rightmost child" rightRaw
  pure (map (\(child, key) ⇒ (child, Just key)) keyed ++ [(right, Nothing)])
  where
    readCell : Nat → Either String (Nat, Integer)
    readCell pointer = do
      if pointer + 4 < database.header.usableSize
        then Right ()
        else Left ("interior cell is truncated on page " ++ show pageNumber)
      childRaw <- big32At ("interior cell on page " ++ show pageNumber) pointer page
      child <- validPageNumber database.header "left child" childRaw
      key <- varintAt "table-interior key" database.header.usableSize (pointer + 4) page
      pure (child, signed64 key.value)

    readCells : List Nat → Either String (List (Nat, Integer))
    readCells [] = Right []
    readCells (pointer :: rest) = do
      cell <- readCell pointer
      later <- readCells rest
      pure (cell :: later)

    strictlyIncreasing : List Integer → Bool
    strictlyIncreasing [] = True
    strictlyIncreasing [_] = True
    strictlyIncreasing (left :: right :: rest) =
      left < right && strictlyIncreasing (right :: rest)

mutual
  walkChildren : SQLiteFile → Nat → List Nat → Maybe Integer →
                 List (Nat, Maybe Integer) →
                 Either String (List Nat, List RawTableRecord)
  walkChildren database fuel seen _ [] = Right (seen, [])
  walkChildren database fuel seen lowerBound ((child, upperBound) :: rest) = do
    (nowSeen, here) <- walkTable database fuel seen child
    (first, last) <- case (here, reverse here) of
      (first :: _, last :: _) ⇒ Right (first, last)
      _ ⇒ Left ("table B-tree child page " ++ show child ++ " contains no rows")
    case lowerBound of
      Nothing ⇒ Right ()
      Just lower ⇒
        if first.rowId > lower
          then Right ()
          else Left
            ("child page " ++ show child ++ " begins at rowid " ++ show first.rowId
              ++ " but must be greater than separator " ++ show lower)
    case upperBound of
      Nothing ⇒ Right ()
      Just upper ⇒
        if last.rowId <= upper
          then Right ()
          else Left
            ("child page " ++ show child ++ " ends at rowid " ++ show last.rowId
              ++ " above separator " ++ show upper)
    let nextLower = case upperBound of
          Nothing ⇒ lowerBound
          Just upper ⇒ Just upper
    (finalSeen, later) <- walkChildren database fuel nowSeen nextLower rest
    pure (finalSeen, here ++ later)

  walkTable : SQLiteFile → Nat → List Nat → Nat →
              Either String (List Nat, List RawTableRecord)
  walkTable database Z seen pageNumber =
    Left "table B-tree depth exceeds database page count"
  walkTable database (S fuel) seen pageNumber = do
    if elem pageNumber seen
      then Left ("cycle or shared page at B-tree page " ++ show pageNumber)
      else Right ()
    page <- pageBytes database pageNumber
    header <- parseHeader pageNumber page
    pointers <- cellPointers database pageNumber header page
    let nowSeen = pageNumber :: seen
    case header.kind of
      TableLeaf ⇒ readLeafCells database pageNumber page nowSeen pointers
      TableInterior ⇒ do
        children <- interiorChildren database pageNumber header page pointers
        walkChildren database fuel nowSeen Nothing children
      other ⇒ Left
        ("expected a table B-tree at page " ++ show pageNumber
          ++ " but found " ++ show other)

orderedRows : List RawTableRecord → Bool
orderedRows [] = True
orderedRows [_] = True
orderedRows (left :: right :: rest) =
  left.rowId < right.rowId && orderedRows (right :: rest)

||| Traverse a table B-tree from `rootPage`, returning raw record payloads in
||| rowid order.  Every B-tree and overflow page is bounds checked and may be
||| visited only once, which turns cycles and shared-page corruption into an
||| error rather than nontermination.
public export
readTableBTree : SQLiteFile → Nat → Either String (List RawTableRecord)
readTableBTree database rootPage = do
  if rootPage > 0 && rootPage <= database.header.pageCount
    then Right ()
    else Left ("invalid table root page " ++ show rootPage)
  (_, records) <- walkTable database database.header.pageCount [] rootPage
  if orderedRows records
    then Right records
    else Left "table B-tree rows are not globally ordered by rowid"

||| Convenience view when the caller needs only `(rowid, raw payload)` pairs.
public export
readTablePayloads : SQLiteFile → Nat → Either String (List (Integer, List Integer))
readTablePayloads database rootPage =
  map (\item ⇒ (item.rowId, item.payload)) <$> readTableBTree database rootPage
