module SQLite.ReadOnly

import Data.List
import SQLite.BTreeRead
import SQLite.File
import SQLite.Record
import SQLite.SQL
import SQLite.Schema
import SQLite.Value
import SQLite.VDBE

%default covering

decodeRows : Nat -> List RawTableRecord -> Either String (List Row)
decodeRows _ [] = Right []
decodeRows width (raw :: rest) = do
  values <- decodeRecord raw.payload
  if length values <= width
    then Right ()
    else Left
      ("rowid " ++ show raw.rowId ++ " contains " ++ show (length values)
        ++ " fields but its schema has " ++ show width)
  later <- decodeRows width rest
  let padded = values ++ replicate (minus width (length values)) SqlNull
  pure (MkRow raw.rowId padded :: later)

rowIdName : String -> Bool
rowIdName name =
  sameName name "rowid" || sameName name "_rowid_" || sameName name "oid"

columnIndex : String -> List Column -> Maybe Nat
columnIndex wanted = findFrom 0
  where
    findFrom : Nat -> List Column -> Maybe Nat
    findFrom _ [] = Nothing
    findFrom index (column :: rest) =
      if sameName wanted column.name
        then Just index
        else findFrom (S index) rest

validateName : List Column -> String -> Either String ()
validateName columns name =
  case columnIndex name columns of
    Just _ => Right ()
    Nothing =>
      if rowIdName name
        then Right ()
        else Left ("no such column: " ++ name)

validateNames : List Column -> List String -> Either String ()
validateNames _ [] = Right ()
validateNames columns (name :: rest) = do
  validateName columns name
  validateNames columns rest

validateProjection : List Column -> Projection -> Either String ()
validateProjection _ AllColumns = Right ()
validateProjection columns (NamedColumns names) = validateNames columns names

validatePredicate : List Column -> Maybe Predicate -> Either String ()
validatePredicate _ Nothing = Right ()
validatePredicate columns (Just (ColumnEquals name _)) = validateName columns name

rowValue : List Column -> String -> Row -> Either String SqlValue
rowValue columns name row =
  case columnIndex name columns of
    Just index =>
      case at index row.values of
        Nothing => Left ("rowid " ++ show row.rowId ++ " is shorter than its schema")
        Just value => Right value
    Nothing =>
      if rowIdName name
        then Right (SqlInteger row.rowId)
        else Left ("no such column: " ++ name)

filterRows : List Column -> Maybe Predicate -> List Row -> Either String (List Row)
filterRows _ Nothing rows = Right rows
filterRows columns (Just (ColumnEquals name wanted)) rows = keep rows
  where
    keep : List Row -> Either String (List Row)
    keep [] = Right []
    keep (row :: rest) = do
      value <- rowValue columns name row
      later <- keep rest
      pure (if sqlEquals value wanted then row :: later else later)

projectNames : List Column -> Projection -> List String
projectNames columns AllColumns = columnNames columns
projectNames _ (NamedColumns names) = names

projectRow : List Column -> Projection -> Row -> Either String (List SqlValue)
projectRow _ AllColumns row = Right row.values
projectRow columns (NamedColumns names) row = choose names
  where
    choose : List String -> Either String (List SqlValue)
    choose [] = Right []
    choose (name :: rest) = do
      value <- rowValue columns name row
      later <- choose rest
      pure (value :: later)

projectRows : List Column -> Projection -> List Row -> Either String (List (List SqlValue))
projectRows _ _ [] = Right []
projectRows columns projection (row :: rest) = do
  here <- projectRow columns projection row
  later <- projectRows columns projection rest
  pure (here :: later)

public export
queryStatement : Statement -> SQLiteFile -> Either String ResultSet
queryStatement (CreateTable _ _) _ = Left "read-only SQLite cannot execute CREATE TABLE"
queryStatement (InsertValues _ _) _ = Left "read-only SQLite cannot execute INSERT"
queryStatement (SelectRows projection tableName predicate) database = do
  schema <- readSchema database
  table <- findTableSchema tableName schema
  columns <- schemaColumns table
  validateProjection columns projection
  validatePredicate columns predicate
  rawRows <- readTableBTree database table.rootPage
  rows <- decodeRows (length columns) rawRows
  matching <- filterRows columns predicate rows
  projected <- projectRows columns projection matching
  pure (MkResultSet (projectNames columns projection) projected)

public export
query : String -> SQLiteFile -> Either String ResultSet
query sql database = do
  tokens <- tokenize sql
  statement <- parse tokens
  queryStatement statement database

public export
queryFile : String -> String -> IO (Either String ResultSet)
queryFile path sql = do
  loaded <- loadSQLiteFile path
  pure (loaded >>= query sql)
