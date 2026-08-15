module SQLite.Database

import SQLite.LeafBTree
import SQLite.SQL
import SQLite.Value

%default covering

public export
data DbError
  = TableAlreadyExists String
  | NoSuchTable String
  | DuplicateColumn String
  | WrongValueCount String Nat Nat
  | NoSuchColumn String
  | InternalError String

public export
Show DbError where
  show (TableAlreadyExists tableName) = "table already exists: " ++ tableName
  show (NoSuchTable tableName) = "no such table: " ++ tableName
  show (DuplicateColumn columnName) = "duplicate column: " ++ columnName
  show (WrongValueCount tableName expected actual) =
    "table " ++ tableName ++ " expects " ++ show expected
      ++ " values, received " ++ show actual
  show (NoSuchColumn columnName) = "no such column: " ++ columnName
  show (InternalError message) = "internal error: " ++ message

public export
record Table where
  constructor MkTable
  tableName : String
  columns : List Column
  nextRowId : Integer
  rows : LeafStore (List SqlValue)

public export
record Database where
  constructor MkDatabase
  leafCapacity : Nat
  tables : List Table

normalCapacity : Nat → Nat
normalCapacity Z = 1
normalCapacity value = value

public export
emptyDatabase : Nat → Database
emptyDatabase requested = MkDatabase (normalCapacity requested) []

public export
findTable : String → Database → Maybe Table
findTable wanted database = findIn database.tables
  where
    findIn : List Table → Maybe Table
    findIn [] = Nothing
    findIn (table :: rest) =
      if sameName wanted table.tableName
        then Just table
        else findIn rest

replaceTable : Table → List Table → List Table
replaceTable updated [] = [updated]
replaceTable updated (table :: rest) =
  if sameName updated.tableName table.tableName
    then updated :: rest
    else table :: replaceTable updated rest

duplicateColumn : List Column → Maybe String
duplicateColumn [] = Nothing
duplicateColumn (column :: rest) =
  if any (\other ⇒ sameName column.name other.name) rest
    then Just column.name
    else duplicateColumn rest

public export
createTable : String → List Column → Database → Either DbError Database
createTable tableName columns database =
  case findTable tableName database of
    Just _ ⇒ Left (TableAlreadyExists tableName)
    Nothing ⇒
      case duplicateColumn columns of
        Just columnName ⇒ Left (DuplicateColumn columnName)
        Nothing ⇒
          let table = MkTable tableName columns 1 (emptyStore database.leafCapacity)
           in Right (MkDatabase database.leafCapacity (database.tables ++ [table]))

public export
insertValues : String → List SqlValue → Database → Either DbError Database
insertValues tableName values database =
  case findTable tableName database of
    Nothing ⇒ Left (NoSuchTable tableName)
    Just table ⇒
      if length values == length table.columns
        then
          let updatedRows = storeInsert table.nextRowId values table.rows
              updated = MkTable table.tableName table.columns
                (table.nextRowId + 1) updatedRows
           in Right (MkDatabase database.leafCapacity
                (replaceTable updated database.tables))
        else Left (WrongValueCount tableName (length table.columns) (length values))

public export
tableRows : Table → List Row
tableRows table = map toRow (storeScan table.rows)
  where
    toRow : Cell (List SqlValue) → Row
    toRow cell = MkRow cell.rowId cell.value
