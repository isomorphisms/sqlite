module SQLite.Schema

import SQLite.BTreeRead
import SQLite.File
import SQLite.Record
import SQLite.SQL
import SQLite.Value

%default covering

||| Text in `sqlite_schema` is normally UTF-8, but SQLite also preserves
||| invalid byte sequences. Keeping those bytes distinct prevents one
||| unsupported identifier from making every other table unreadable.
public export
data SchemaText
  = Utf8SchemaText String
  | RawSchemaText (List Integer)

public export
Eq SchemaText where
  Utf8SchemaText left == Utf8SchemaText right = left == right
  RawSchemaText left == RawSchemaText right = left == right
  _ == _ = False

public export
Show SchemaText where
  show (Utf8SchemaText text) = show text
  show (RawSchemaText bytes) = "TEXT BYTES " ++ show bytes

public export
record SchemaEntry where
  constructor MkSchemaEntry
  objectType : SchemaText
  name : SchemaText
  tableName : SchemaText
  rootPage : Nat
  createSql : Maybe SchemaText

public export
Eq SchemaEntry where
  left == right =
    left.objectType == right.objectType
      && left.name == right.name
      && left.tableName == right.tableName
      && left.rootPage == right.rootPage
      && left.createSql == right.createSql

public export
Show SchemaEntry where
  show entry =
    show entry.objectType ++ " " ++ show entry.name
      ++ " on page " ++ show entry.rootPage

schemaText : String -> Integer -> SqlValue -> Either String SchemaText
schemaText _ _ (SqlText text) = Right (Utf8SchemaText text)
schemaText _ _ (SqlTextBytes bytes) = Right (RawSchemaText bytes)
schemaText field rowId _ = Left
  ("sqlite_schema row " ++ show rowId ++ " has non-text " ++ field)

schemaEntry : RawTableRecord -> Either String SchemaEntry
schemaEntry raw = do
  values <- decodeRecord raw.payload
  case values of
    [objectTypeValue, nameValue, tableNameValue, SqlInteger rootPage, sqlValue] => do
       objectType <- schemaText "object type" raw.rowId objectTypeValue
       name <- schemaText "name" raw.rowId nameValue
       tableName <- schemaText "table name" raw.rowId tableNameValue
       if rootPage >= 0
         then Right ()
         else Left ("sqlite_schema row " ++ show raw.rowId ++ " has a negative root page")
       createSql <- case sqlValue of
         SqlNull => Right Nothing
         SqlText sql => Right (Just (Utf8SchemaText sql))
         SqlTextBytes bytes => Right (Just (RawSchemaText bytes))
         _ => Left ("sqlite_schema row " ++ show raw.rowId ++ " has non-text SQL")
       pure (MkSchemaEntry objectType name tableName (integerToNat rootPage) createSql)
    _ => Left
      ("sqlite_schema row " ++ show raw.rowId
        ++ " does not have its required five fields")

decodeEntries : List RawTableRecord -> Either String (List SchemaEntry)
decodeEntries [] = Right []
decodeEntries (raw :: rest) = do
  entry <- schemaEntry raw
  later <- decodeEntries rest
  pure (entry :: later)

public export
readSchema : SQLiteFile -> Either String (List SchemaEntry)
readSchema database = do
  records <- readTableBTree database 1
  decodeEntries records

public export
findTableSchema : String -> List SchemaEntry -> Either String SchemaEntry
findTableSchema wanted [] = Left ("no such table: " ++ wanted)
findTableSchema wanted (entry :: rest) =
  case (entry.objectType, entry.name) of
    (Utf8SchemaText objectType, Utf8SchemaText name) =>
      if sameName objectType "table" && sameName name wanted
        then
          if entry.rootPage > 0
            then Right entry
            else Left ("table " ++ wanted ++ " has no B-tree root page")
        else findTableSchema wanted rest
    _ => findTableSchema wanted rest

public export
schemaColumns : SchemaEntry -> Either String (List Column)
schemaColumns entry =
  case (entry.name, entry.createSql) of
    (RawSchemaText _, _) =>
      Left "table name in sqlite_schema is not valid UTF-8"
    (Utf8SchemaText name, Nothing) =>
      Left ("table " ++ name ++ " has no CREATE statement")
    (Utf8SchemaText name, Just (RawSchemaText _)) =>
      Left ("CREATE statement for " ++ name ++ " is not valid UTF-8")
    (Utf8SchemaText name, Just (Utf8SchemaText sql)) => do
      tokens <- tokenize sql
      statement <- parse tokens
      case statement of
        CreateTable parsedName columns =>
          if sameName parsedName name
            then Right columns
            else Left
              ("schema SQL names table " ++ parsedName
                ++ " but sqlite_schema names " ++ name)
        _ => Left ("schema SQL for " ++ name ++ " is not CREATE TABLE")
