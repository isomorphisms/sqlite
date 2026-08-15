module SQLite.Page

import SQLite.Value

%default total

public export
data PageKind
  = IndexInterior
  | TableInterior
  | IndexLeaf
  | TableLeaf

public export
Eq PageKind where
  IndexInterior == IndexInterior = True
  TableInterior == TableInterior = True
  IndexLeaf == IndexLeaf = True
  TableLeaf == TableLeaf = True
  _ == _ = False

public export
Show PageKind where
  show IndexInterior = "index interior"
  show TableInterior = "table interior"
  show IndexLeaf = "index leaf"
  show TableLeaf = "table leaf"

public export
record PageHeader where
  constructor MkPageHeader
  kind : PageKind
  firstFreeblock : Integer
  cellCount : Integer
  cellContentStart : Integer
  fragmentedFreeBytes : Integer
  rightmostChild : Maybe Integer

public export
Eq PageHeader where
  left == right =
    left.kind == right.kind
      && left.firstFreeblock == right.firstFreeblock
      && left.cellCount == right.cellCount
      && left.cellContentStart == right.cellContentStart
      && left.fragmentedFreeBytes == right.fragmentedFreeBytes
      && left.rightmostChild == right.rightmostChild

public export
Show PageHeader where
  show header =
    show header.kind
      ++ ", " ++ show header.cellCount ++ " cells"
      ++ ", content starts at " ++ show header.cellContentStart

readByte : Nat -> List Integer -> Either String Integer
readByte offset bytes =
  case at offset bytes of
    Nothing => Left ("page ends before byte " ++ show offset)
    Just byte =>
      if byte >= 0 && byte <= 255
        then Right byte
        else Left ("page contains a non-byte value at offset " ++ show offset)

readBig16 : Nat -> List Integer -> Either String Integer
readBig16 offset bytes = do
  high <- readByte offset bytes
  low <- readByte (S offset) bytes
  pure (high * 256 + low)

readBig32 : Nat -> List Integer -> Either String Integer
readBig32 offset bytes = do
  first <- readByte offset bytes
  second <- readByte (offset + 1) bytes
  third <- readByte (offset + 2) bytes
  fourth <- readByte (offset + 3) bytes
  pure (first * 16777216 + second * 65536 + third * 256 + fourth)

pageKind : Integer -> Either String PageKind
pageKind 2 = Right IndexInterior
pageKind 5 = Right TableInterior
pageKind 10 = Right IndexLeaf
pageKind 13 = Right TableLeaf
pageKind other = Left ("unknown SQLite B-tree page type " ++ show other)

isInterior : PageKind -> Bool
isInterior IndexInterior = True
isInterior TableInterior = True
isInterior _ = False

public export
parseHeader : Nat -> List Integer -> Either String PageHeader
parseHeader pageNumber bytes = do
  let offset = if pageNumber == 1 then 100 else 0
  rawKind <- readByte offset bytes
  kind <- pageKind rawKind
  freeblock <- readBig16 (offset + 1) bytes
  cells <- readBig16 (offset + 3) bytes
  rawContentStart <- readBig16 (offset + 5) bytes
  fragments <- readByte (offset + 7) bytes
  if fragments <= 60
    then Right ()
    else Left ("B-tree page reports more than 60 fragmented free bytes")
  rightmost <-
    if isInterior kind
      then map Just (readBig32 (offset + 8) bytes)
      else Right Nothing
  let contentStart = if rawContentStart == 0 then 65536 else rawContentStart
  pure (MkPageHeader kind freeblock cells contentStart fragments rightmost)
