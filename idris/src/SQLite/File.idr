module SQLite.File

import Data.Buffer
import Data.List
import Data.Nat
import System.File.Buffer

%default covering

||| The parts of the 100-byte format-3 database header needed by the
||| read-only B-tree reader.  Page counts distinguish the logical database
||| size from the number of complete pages physically present in the file.
||| The constructor stays private so callers cannot forge a validated header.
export
data DatabaseHeader = MkDatabaseHeader
  Nat Nat Nat Nat Nat Nat Integer Integer Integer Integer Integer

public export
(.pageSize) : DatabaseHeader → Nat
(.pageSize) (MkDatabaseHeader value _ _ _ _ _ _ _ _ _ _) = value

public export
(.reservedBytes) : DatabaseHeader → Nat
(.reservedBytes) (MkDatabaseHeader _ value _ _ _ _ _ _ _ _ _) = value

public export
(.usableSize) : DatabaseHeader → Nat
(.usableSize) (MkDatabaseHeader _ _ value _ _ _ _ _ _ _ _) = value

public export
(.pageCount) : DatabaseHeader → Nat
(.pageCount) (MkDatabaseHeader _ _ _ value _ _ _ _ _ _ _) = value

public export
(.filePageCount) : DatabaseHeader → Nat
(.filePageCount) (MkDatabaseHeader _ _ _ _ value _ _ _ _ _ _) = value

public export
(.headerPageCount) : DatabaseHeader → Nat
(.headerPageCount) (MkDatabaseHeader _ _ _ _ _ value _ _ _ _ _) = value

public export
(.writeVersion) : DatabaseHeader → Integer
(.writeVersion) (MkDatabaseHeader _ _ _ _ _ _ value _ _ _ _) = value

public export
(.readVersion) : DatabaseHeader → Integer
(.readVersion) (MkDatabaseHeader _ _ _ _ _ _ _ value _ _ _) = value

public export
(.schemaFormat) : DatabaseHeader → Integer
(.schemaFormat) (MkDatabaseHeader _ _ _ _ _ _ _ _ value _ _) = value

public export
(.changeCounter) : DatabaseHeader → Integer
(.changeCounter) (MkDatabaseHeader _ _ _ _ _ _ _ _ _ value _) = value

public export
(.versionValidFor) : DatabaseHeader → Integer
(.versionValidFor) (MkDatabaseHeader _ _ _ _ _ _ _ _ _ _ value) = value

public export
Show DatabaseHeader where
  show header =
    "SQLite format 3, " ++ show header.pageSize ++ "-byte pages, "
      ++ show header.usableSize ++ " usable bytes, "
      ++ show header.pageCount ++ " pages, UTF-8"

||| A validated, immutable SQLite file image.  Bytes are Integers so all
||| parsing after the single Buffer-to-List conversion remains pure.  Its
||| constructor is private: public values originate at `parseSQLiteFile`.
export
data SQLiteFile = MkSQLiteFile DatabaseHeader (List Integer)

public export
(.header) : SQLiteFile → DatabaseHeader
(.header) (MkSQLiteFile value _) = value

public export
(.contents) : SQLiteFile → List Integer
(.contents) (MkSQLiteFile _ value) = value

sqliteMagic : List Integer
sqliteMagic =
  [83, 81, 76, 105, 116, 101, 32, 102,
   111, 114, 109, 97, 116, 32, 51, 0]

validByte : Integer → Bool
validByte byte = byte >= 0 && byte <= 255

byteAt : Nat → List Integer → Either String Integer
byteAt offset bytes =
  case getAt offset bytes of
    Nothing ⇒ Left ("database header ends before byte " ++ show offset)
    Just byte ⇒
      if validByte byte
        then Right byte
        else Left ("value at byte " ++ show offset ++ " is outside 0..255")

big16At : Nat → List Integer → Either String Integer
big16At offset bytes = do
  high <- byteAt offset bytes
  low <- byteAt (offset + 1) bytes
  pure (high * 256 + low)

big32At : Nat → List Integer → Either String Integer
big32At offset bytes = do
  first <- byteAt offset bytes
  second <- byteAt (offset + 1) bytes
  third <- byteAt (offset + 2) bytes
  fourth <- byteAt (offset + 3) bytes
  pure (first * 16777216 + second * 65536 + third * 256 + fourth)

validPageSize : Integer → Bool
validPageSize size =
  elem size [512, 1024, 2048, 4096, 8192, 16384, 32768, 65536]

natural : Integer → Nat
natural = integerToNat

logicalPageCount : Integer → Integer → Integer → Nat → Either String Nat
logicalPageCount changed declared validFor physical =
  if changed == validFor && declared > 0
    then
      if declared <= natToInteger physical
        then Right (natural declared)
        else Left
          ("header declares " ++ show declared ++ " pages but the file has only "
            ++ show physical)
    else Right physical

||| Validate the format-3 header and retain a pure file image.  This reader is
||| deliberately strict about the standard payload fractions and UTF-8 text
||| encoding.  The logical page count follows SQLite's change-counter rule.
public export
parseSQLiteFile : List Integer → Either String SQLiteFile
parseSQLiteFile bytes = do
  if length bytes < 100
    then Left "file is shorter than SQLite's 100-byte database header"
    else Right ()
  if all validByte bytes
    then Right ()
    else Left "file image contains a value outside the byte range 0..255"
  if take 16 bytes == sqliteMagic
    then Right ()
    else Left "missing SQLite format 3 signature"

  rawPageSize <- big16At 16 bytes
  let pageBytes = if rawPageSize == 1 then 65536 else rawPageSize
  if validPageSize pageBytes
    then Right ()
    else Left ("invalid SQLite page size " ++ show pageBytes)

  writer <- byteAt 18 bytes
  reader <- byteAt 19 bytes
  if writer == 1 && reader == 1
    then Right ()
    else Left
      "WAL-mode database headers are not safe to read without applying the WAL"

  reserved <- byteAt 20 bytes
  let usable = pageBytes - reserved
  if usable >= 480
    then Right ()
    else Left ("usable page size is only " ++ show usable ++ " bytes")

  maximumFraction <- byteAt 21 bytes
  minimumFraction <- byteAt 22 bytes
  leafFraction <- byteAt 23 bytes
  if (maximumFraction, minimumFraction, leafFraction) == (64, 32, 32)
    then Right ()
    else Left "non-standard SQLite payload fractions in database header"

  let pageBytesNat = natural pageBytes
  if length bytes `mod` pageBytesNat == 0
    then Right ()
    else Left "file length is not a whole number of database pages"
  let physicalPages = length bytes `div` pageBytesNat
  if physicalPages > 0
    then Right ()
    else Left "database contains no complete page"

  changed <- big32At 24 bytes
  declared <- big32At 28 bytes
  schema <- big32At 44 bytes
  encoding <- big32At 56 bytes
  if schema == 0 && encoding == 0
    then do
      pageKind <- byteAt 100 bytes
      cells <- big16At 103 bytes
      if physicalPages == 1 && pageKind == 13 && cells == 0
        then Right ()
        else Left "zero schema format is valid only for an empty initialized database"
    else do
      if elem schema [1, 2, 3, 4]
        then Right ()
        else Left ("invalid SQLite schema format " ++ show schema)
      if encoding == 1
        then Right ()
        else Left ("database text encoding is not UTF-8 (code " ++ show encoding ++ ")")
  validFor <- big32At 92 bytes
  logicalPages <- logicalPageCount changed declared validFor physicalPages

  let parsedHeader = MkDatabaseHeader
        pageBytesNat
        (natural reserved)
        (natural usable)
        logicalPages
        physicalPages
        (natural declared)
        writer
        reader
        schema
        changed
        validFor
  pure (MkSQLiteFile parsedHeader bytes)

||| Return one complete page.  SQLite page numbers are one-based.
public export
pageBytes : SQLiteFile → Nat → Either String (List Integer)
pageBytes database Z = Left "SQLite page number 0 is invalid"
pageBytes database (S zeroBased) =
  if S zeroBased > database.header.pageCount
    then Left
      ("page " ++ show (S zeroBased) ++ " is outside database bounds 1.."
        ++ show database.header.pageCount)
    else
      let start = zeroBased * database.header.pageSize
          answer = take database.header.pageSize (drop start database.contents)
       in if length answer == database.header.pageSize
            then Right answer
            else Left ("physical file ends inside page " ++ show (S zeroBased))

||| Read a file into an Idris Buffer once, convert it to pure integer bytes,
||| and run the same validation as `parseSQLiteFile`.
public export
loadSQLiteFile : String → IO (Either String SQLiteFile)
loadSQLiteFile path = do
  result <- createBufferFromFile path
  case result of
    Left error ⇒ pure (Left ("could not read " ++ path ++ ": " ++ show error))
    Right buffer ⇒ do
      bytes <- bufferData' buffer
      pure (parseSQLiteFile (map cast bytes))
