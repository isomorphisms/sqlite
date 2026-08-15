module SQLite.Value

%default total

public export
data Affinity
  = IntegerAffinity
  | RealAffinity
  | TextAffinity
  | BlobAffinity

public export
Eq Affinity where
  IntegerAffinity == IntegerAffinity = True
  RealAffinity == RealAffinity = True
  TextAffinity == TextAffinity = True
  BlobAffinity == BlobAffinity = True
  _ == _ = False

public export
Show Affinity where
  show IntegerAffinity = "INTEGER"
  show RealAffinity = "REAL"
  show TextAffinity = "TEXT"
  show BlobAffinity = "BLOB"

public export
record Column where
  constructor MkColumn
  name : String
  affinity : Affinity

public export
Eq Column where
  left == right = left.name == right.name && left.affinity == right.affinity

public export
Show Column where
  show column = column.name ++ " " ++ show column.affinity

public export
data SqlValue
  = SqlNull
  | SqlInteger Integer
  | SqlReal Double
  | SqlText String
  | SqlTextBytes (List Integer)
  | SqlBlob (List Integer)

public export
Eq SqlValue where
  SqlNull == SqlNull = True
  SqlInteger left == SqlInteger right = left == right
  SqlReal left == SqlReal right = left == right
  SqlText left == SqlText right = left == right
  SqlTextBytes left == SqlTextBytes right = left == right
  SqlBlob left == SqlBlob right = left == right
  _ == _ = False

escapeTextCharacter : Char -> String
escapeTextCharacter '\'' = "''"
escapeTextCharacter '\\' = "\\\\"
escapeTextCharacter '\0' = "\\0"
escapeTextCharacter '\n' = "\\n"
escapeTextCharacter '\r' = "\\r"
escapeTextCharacter '\t' = "\\t"
escapeTextCharacter character =
  if ord character < 32 || (ord character >= 127 && ord character <= 159)
    then "\\codepoint{" ++ show (ord character) ++ "}"
    else pack [character]

escapeText : List Char -> String
escapeText [] = ""
escapeText (character :: rest) =
  escapeTextCharacter character ++ escapeText rest

public export
Show SqlValue where
  show SqlNull = "NULL"
  show (SqlInteger value) = show value
  show (SqlReal value) = show value
  show (SqlText value) = "'" ++ escapeText (unpack value) ++ "'"
  show (SqlTextBytes bytes) = "TEXT BYTES " ++ show bytes
  show (SqlBlob bytes) = "BLOB " ++ show bytes

||| The small query subset has only SQL `=`.  NULL never compares equal,
||| including to itself.  Numeric affinity and collation coercions are not yet
||| modeled; all other values use constructor-preserving equality.
public export
sqlEquals : SqlValue -> SqlValue -> Bool
sqlEquals SqlNull _ = False
sqlEquals _ SqlNull = False
sqlEquals left right = left == right

public export
record Row where
  constructor MkRow
  rowId : Integer
  values : List SqlValue

public export
Eq Row where
  left == right = left.rowId == right.rowId && left.values == right.values

public export
Show Row where
  show row = show row.rowId ++ ": " ++ show row.values

public export
at : Nat -> List element -> Maybe element
at Z (value :: _) = Just value
at (S index) (_ :: rest) = at index rest
at _ [] = Nothing

public export
columnPosition : String -> List Column -> Maybe Nat
columnPosition wanted = findFrom Z
  where
    findFrom : Nat -> List Column -> Maybe Nat
    findFrom _ [] = Nothing
    findFrom index (column :: rest) =
      if column.name == wanted
        then Just index
        else findFrom (S index) rest

public export
columnNames : List Column -> List String
columnNames = map name
