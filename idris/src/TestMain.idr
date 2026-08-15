module TestMain

import Data.List
import SQLite.BTreeRead
import SQLite.Database
import SQLite.Engine
import SQLite.File
import SQLite.LeafBTree
import SQLite.Page
import SQLite.ReadOnly
import SQLite.Record
import SQLite.Schema
import SQLite.SQL
import SQLite.Value
import SQLite.Varint
import SQLite.VDBE
import System

%default covering

assertEqual : (Eq item, Show item) => String -> item -> item -> Either String ()
assertEqual name expected actual =
  if expected == actual
    then Right ()
    else Left (name ++ "\n  expected: " ++ show expected ++ "\n  actual:   " ++ show actual)

assertTrue : String -> Bool -> Either String ()
assertTrue name = assertEqual name True

assertLeft : String -> String -> Either String answer -> Either String ()
assertLeft name expected (Left actual) = assertEqual name expected actual
assertLeft name _ (Right _) = Left (name ++ "\n  expected an error, received success")

assertFails : String -> Either String answer -> Either String ()
assertFails _ (Left _) = Right ()
assertFails name (Right _) = Left (name ++ "\n  expected an error, received success")

replaceAt : Nat -> element -> List element -> List element
replaceAt Z replacement (_ :: rest) = replacement :: rest
replaceAt (S index) replacement (value :: rest) =
  value :: replaceAt index replacement rest
replaceAt _ _ [] = []

varintCase : Integer -> List Integer -> Either String ()
varintCase value bytes = do
  assertEqual ("encode varint " ++ show value) (Right bytes) (encode value)
  assertEqual ("decode varint " ++ show value)
    (Right (MkDecodedVarint value (length bytes) []))
    (decode bytes)

varintTests : List (Either String ())
varintTests =
  [ varintCase 0 [0]
  , varintCase 127 [127]
  , varintCase 128 [129, 0]
  , varintCase 16383 [255, 127]
  , varintCase 16384 [129, 128, 0]
  , varintCase 72057594037927936 [128, 192, 128, 128, 128, 128, 128, 128, 0]
  , varintCase 18446744073709551615 [255, 255, 255, 255, 255, 255, 255, 255, 255]
  , assertEqual "decode preserves unconsumed bytes"
      (Right (MkDecodedVarint 1 1 [99, 100]))
      (decode [1, 99, 100])
  , assertEqual "reject truncated varint"
      (Left "truncated SQLite varint")
      (decode [128])
  ]

pageTests : List (Either String ())
pageTests =
  [ assertEqual "table leaf header"
      (Right (MkPageHeader TableLeaf 0 2 32 1 Nothing))
      (parseHeader 2 [13, 0, 0, 0, 2, 0, 32, 1])
  , assertEqual "table interior header"
      (Right (MkPageHeader TableInterior 0 1 20 0 (Just 9)))
      (parseHeader 2 [5, 0, 0, 0, 1, 0, 20, 0, 0, 0, 0, 9])
  , assertEqual "page-one B-tree header begins after database header"
      (Right (MkPageHeader TableLeaf 0 0 4096 0 Nothing))
      (parseHeader 1 (replicate 100 0 ++ [13, 0, 0, 0, 0, 16, 0, 0]))
  ]

leafStoreTests : List (Either String ())
leafStoreTests =
  let store = storeInsert 3 "three"
            $ storeInsert 1 "one"
            $ storeInsert 2 "two"
            $ emptyStore 2
   in [ assertTrue "leaf store invariant" (validStore store)
      , assertEqual "leaf pages split at configured capacity" 2 (length store.pages)
      , assertEqual "leaf scan is ordered" [1, 2, 3] (map rowId (storeScan store))
      , assertEqual "leaf lookup" (Just "two") (storeLookup 2 store)
      , assertTrue "public zero-capacity store is normalized on insertion"
          (validStore (storeInsert 1 "one" (MkLeafStore 0 [])))
      ]

sqlTests : List (Either String ())
sqlTests =
  [ assertEqual "tokenizer handles quotes and punctuation"
      (Right [WordToken "INSERT", WordToken "INTO", WordToken "t",
              WordToken "VALUES", LeftParenToken, TextToken "don't", CommaToken,
              IntegerToken "-2", RightParenToken, SemicolonToken])
      (tokenize "INSERT INTO t VALUES ('don''t', -2);")
  , assertTrue "parser accepts mixed-case keywords"
      (case tokenize "select name FROM people WHERE age = 85;" of
        Left _ => False
        Right tokens => parse tokens == Right
          (SelectRows (NamedColumns ["name"]) "people"
            (Just (ColumnEquals "age" (SqlInteger 85)))))
  ]

recordTests : List (Either String ())
recordTests =
  [ assertEqual "record integers, UTF-8 text, and NULL"
      (Right [SqlInteger (-1), SqlText "λ", SqlNull])
      (decodeRecord [4, 1, 17, 0, 255, 206, 187])
  , assertEqual "record IEEE-754 real"
      (Right [SqlReal 9.5])
      (decodeRecord [2, 7, 64, 35, 0, 0, 0, 0, 0, 0])
  , assertEqual "record reserved serial type"
      (Left "SQLite serial type 10 is reserved")
      (decodeRecord [2, 10])
  , assertEqual "record NaN is SQL NULL"
      (Right [SqlNull])
      (decodeRecord [2, 7, 127, 248, 0, 0, 0, 0, 0, 1])
  , assertEqual "invalid UTF-8 text bytes remain text bytes"
      (Right [SqlTextBytes [128]])
      (decodeRecord [2, 15, 128])
  , assertEqual "reject overlong UTF-8"
      (Left "invalid UTF-8 leading byte 192")
      (decodeUtf8 [192, 128])
  ]

engineTest : Either String ()
engineTest =
  case executeMany
    [ "CREATE TABLE people (name TEXT, age INTEGER);"
    , "INSERT INTO people VALUES ('Ada', 36);"
    , "INSERT INTO people VALUES ('Grace', 85);"
    , "SELECT name FROM people WHERE age = 85;"
    ] (emptyDatabase 1) of
      Left error => Left ("engine scenario failed: " ++ error)
      Right (_, results) => assertEqual "SQL -> VDBE -> leaf store result"
        [MkResultSet ["name"] [[SqlText "Grace"]]] results

allTests : List (Either String ())
allTests = varintTests ++ pageTests ++ leafStoreTests ++ sqlTests ++ recordTests ++ [engineTest]

runTests : Nat -> List (Either String ()) -> Either String Nat
runTests passed [] = Right passed
runTests passed (Left error :: _) = Left error
runTests passed (Right () :: rest) = runTests (S passed) rest

basicFixtureTests : SQLiteFile -> List (Either String ())
basicFixtureTests database =
  [ assertEqual "fixture page size" 512 database.header.pageSize
  , assertTrue "WAL header is rejected without a WAL snapshot"
      (case parseSQLiteFile
        (take 18 database.contents ++ [2, 2] ++ drop 20 database.contents) of
          Left message => message ==
            "WAL-mode database headers are not safe to read without applying the WAL"
          Right _ => False)
  , assertEqual "fixture schema"
      (Right [MkSchemaEntry
        (Utf8SchemaText "table")
        (Utf8SchemaText "people")
        (Utf8SchemaText "people")
        2
        (Just (Utf8SchemaText
          "CREATE TABLE people(name TEXT, age INTEGER, score REAL, note BLOB)"))])
      (readSchema database)
  , assertEqual "real SQLite file query"
      (Right (MkResultSet ["rowid", "name", "age", "score", "note"]
        [ [SqlInteger 1, SqlText "Ada", SqlInteger 36, SqlReal 9.5,
            SqlBlob [0, 255]]
        , [SqlInteger 2, SqlText "Grace", SqlInteger 85, SqlNull, SqlBlob []]
        , [SqlInteger 3, SqlText "Évariste", SqlInteger 20, SqlReal (-1.25),
            SqlBlob [202, 254, 186, 190]]
        ]))
      (query "SELECT rowid, name, age, score, note FROM people;" database)
  , assertEqual "rowid filter on real file"
      (Right (MkResultSet ["name"] [[SqlText "Grace"]]))
      (query "SELECT name FROM people WHERE rowid = 2;" database)
  , assertEqual "SQL NULL does not equal itself"
      (Right (MkResultSet ["name"] []))
      (query "SELECT name FROM people WHERE score = NULL;" database)
  , case parseSQLiteFile
      (replaceAt (database.header.pageSize + 7) 61 database.contents) of
      Left error => Left error
      Right corrupted => assertLeft "fragmented-byte corruption"
        "B-tree page reports more than 60 fragmented free bytes"
        (query "SELECT name FROM people;" corrupted)
  ]

multipageFixtureTests : SQLiteFile -> List (Either String ())
multipageFixtureTests database =
  [ case query "SELECT n, label FROM nums;" database of
      Left error => Left error
      Right result => do
        assertEqual "multi-page table row count" 200 (length result.rows)
        assertEqual "multi-page first row"
          (Just [SqlInteger 1, SqlText "row-001"]) (at 0 result.rows)
        assertEqual "multi-page last row"
          (Just [SqlInteger 200, SqlText "row-200"]) (at 199 result.rows)
  , case parseSQLiteFile (replaceAt 1023 34 database.contents) of
      Left error => Left error
      Right corrupted => assertFails "interior separator must bound adjacent children"
        (query "SELECT n FROM nums;" corrupted)
  ]

deletedFixtureTests : SQLiteFile -> List (Either String ())
deletedFixtureTests database =
  [ case query "SELECT n FROM nums;" database of
      Left error => Left error
      Right result => assertEqual
        "a valid stale interior separator survives deleted boundary rows"
        189 (length result.rows)
  ]

overflowFixtureTests : SQLiteFile -> List (Either String ())
overflowFixtureTests database =
  [ case query "SELECT body FROM docs;" database of
      Left error => Left error
      Right (MkResultSet _ [[SqlText body]]) => do
        assertEqual "overflow record character count" 2002 (length (unpack body))
        assertEqual "overflow record UTF-8 tail" ['λ', ' '] (take 2 (reverse (unpack body)))
      Right other => Left ("unexpected overflow query result " ++ show other)
  ]

alteredFixtureTests : SQLiteFile -> List (Either String ())
alteredFixtureTests database =
  [ assertEqual "ALTER TABLE synthesizes a trailing NULL for old records"
      (Right (MkResultSet ["a", "c"]
        [[SqlInteger 1, SqlNull], [SqlInteger 2, SqlText "new"]]))
      (query "SELECT a, c FROM t;" database)
  ]

invalidTextFixtureTests : SQLiteFile -> List (Either String ())
invalidTextFixtureTests database =
  [ assertEqual "valid SQLite file may contain invalid UTF-8 text bytes"
      (Right (MkResultSet ["value"] [[SqlTextBytes [128]]]))
      (query "SELECT value FROM t;" database)
  ]

invalidSchemaFixtureTests : SQLiteFile -> List (Either String ())
invalidSchemaFixtureTests database =
  [ assertEqual "an unrelated invalid schema identifier does not poison a table"
      (Right (MkResultSet ["a"] [[SqlInteger 9]]))
      (query "SELECT a FROM t;" database)
  , case readSchema database of
      Left error => Left error
      Right entries =>
        case at 1 entries of
          Just entry => assertEqual "invalid schema name bytes are retained"
            (RawSchemaText [98, 97, 100, 128, 110, 97, 109, 101])
            entry.name
          Nothing => Left "invalid-schema fixture has no second schema row"
  ]

controlTextFixtureTests : SQLiteFile -> List (Either String ())
controlTextFixtureTests database =
  [ case query "SELECT s FROM t;" database of
      Left error => Left error
      Right result => do
        assertEqual "embedded NUL remains in the decoded value"
          (MkResultSet ["s"] [[SqlText "A\0B\n'"]]) result
        assertTrue "result rendering escapes embedded NUL"
          (not (elem '\0' (unpack (show result))))
  ]

emptyDatabaseFixtureTests : SQLiteFile -> List (Either String ())
emptyDatabaseFixtureTests database =
  [ assertEqual "initialized empty format-3 database has no schema rows"
      (Right []) (readSchema database)
  ]

emptyTableFixtureTests : SQLiteFile -> List (Either String ())
emptyTableFixtureTests database =
  [ assertEqual "empty table query"
      (Right (MkResultSet ["a"] []))
      (query "SELECT a FROM t;" database)
  , assertLeft "empty-table projection still validates columns"
      "no such column: bogus"
      (query "SELECT bogus FROM t;" database)
  , assertLeft "empty-table predicate still validates columns"
      "no such column: bogus"
      (query "SELECT a FROM t WHERE bogus = 1;" database)
  ]

runFixture : Nat -> String -> (SQLiteFile -> List (Either String ())) -> IO (Either String Nat)
runFixture passed path tests = do
  loaded <- loadSQLiteFile path
  pure $ case loaded of
    Left error => Left error
    Right database => runTests passed (tests database)

record FixtureCase where
  constructor MkFixtureCase
  path : String
  checks : SQLiteFile -> List (Either String ())

fixtureCases : List FixtureCase
fixtureCases =
  [ MkFixtureCase "fixtures/basic.db" basicFixtureTests
  , MkFixtureCase "fixtures/multipage.db" multipageFixtureTests
  , MkFixtureCase "fixtures/deleted.db" deletedFixtureTests
  , MkFixtureCase "fixtures/overflow.db" overflowFixtureTests
  , MkFixtureCase "fixtures/altered.db" alteredFixtureTests
  , MkFixtureCase "fixtures/invalid-text.db" invalidTextFixtureTests
  , MkFixtureCase "fixtures/invalid-schema.db" invalidSchemaFixtureTests
  , MkFixtureCase "fixtures/control-text.db" controlTextFixtureTests
  , MkFixtureCase "fixtures/empty.db" emptyDatabaseFixtureTests
  , MkFixtureCase "fixtures/empty-table.db" emptyTableFixtureTests
  ]

fixtureTests : Nat -> List FixtureCase -> IO (Either String Nat)
fixtureTests passed [] = pure (Right passed)
fixtureTests passed (fixture :: rest) = do
  here <- runFixture passed fixture.path fixture.checks
  case here of
    Left error => pure (Left error)
    Right later => fixtureTests later rest

main : IO ()
main =
  case runTests 0 allTests of
    Left error => die ("FAIL\n" ++ error)
    Right pureCount => do
      fixtures <- fixtureTests pureCount fixtureCases
      case fixtures of
        Left error => die ("FAIL\n" ++ error)
        Right count => putStrLn ("PASS: " ++ show count ++ " ordinary-Idris tests")
