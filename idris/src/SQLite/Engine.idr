module SQLite.Engine

import SQLite.Compiler
import SQLite.Database
import SQLite.SQL
import SQLite.VDBE

%default covering

public export
execute : String -> Database -> Either String (Database, Maybe ResultSet)
execute sql database = do
  tokens <- tokenize sql
  statement <- parse tokens
  case runProgram (compile statement) database of
    Left error => Left (show error)
    Right answer => Right answer

public export
executeMany : List String -> Database -> Either String (Database, List ResultSet)
executeMany [] database = Right (database, [])
executeMany (sql :: rest) database = do
  (later, possibleResult) <- execute sql database
  (final, results) <- executeMany rest later
  pure (final, maybe results (:: results) possibleResult)
