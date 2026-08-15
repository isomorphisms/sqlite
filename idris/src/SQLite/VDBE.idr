module SQLite.VDBE

import SQLite.Database
import SQLite.SQL
import SQLite.Value

%default covering

public export
data Instruction
  = CreateTableOp String (List Column)
  | InsertOp String (List SqlValue)
  | OpenReadOp String
  | FilterEqualsOp String SqlValue
  | ProjectOp Projection
  | ResultRowsOp
  | HaltOp

public export
Show Instruction where
  show (CreateTableOp name columns) = "CreateTable " ++ name ++ " " ++ show columns
  show (InsertOp name values) = "Insert " ++ name ++ " " ++ show values
  show (OpenReadOp name) = "OpenRead " ++ name
  show (FilterEqualsOp name value) = "Filter " ++ name ++ " = " ++ show value
  show (ProjectOp AllColumns) = "Project *"
  show (ProjectOp (NamedColumns names)) = "Project " ++ show names
  show ResultRowsOp = "ResultRows"
  show HaltOp = "Halt"

public export
record ResultSet where
  constructor MkResultSet
  columns : List String
  rows : List (List SqlValue)

public export
Eq ResultSet where
  left == right = left.columns == right.columns && left.rows == right.rows

public export
Show ResultSet where
  show result = show result.columns ++ "\n" ++ joinLines (map show result.rows)
    where
      joinLines : List String -> String
      joinLines [] = ""
      joinLines (line :: rest) = line ++ "\n" ++ joinLines rest

record Machine where
  constructor MkMachine
  database : Database
  activeColumns : List Column
  activeRows : List Row
  output : Maybe ResultSet
  halted : Bool

initialMachine : Database -> Machine
initialMachine database = MkMachine database [] [] Nothing False

position : String -> List Column -> Maybe Nat
position wanted = findFrom 0
  where
    findFrom : Nat -> List Column -> Maybe Nat
    findFrom _ [] = Nothing
    findFrom index (column :: rest) =
      if sameName wanted column.name
        then Just index
        else findFrom (S index) rest

positions : List String -> List Column -> Either DbError (List Nat)
positions [] _ = Right []
positions (name :: rest) columns =
  case position name columns of
    Nothing => Left (NoSuchColumn name)
    Just index => do
      later <- positions rest columns
      pure (index :: later)

pick : List Nat -> List value -> Either DbError (List value)
pick [] _ = Right []
pick (index :: rest) values =
  case at index values of
    Nothing => Left (InternalError "row is shorter than its table schema")
    Just value => do
      later <- pick rest values
      pure (value :: later)

filterRows : Nat -> SqlValue -> List Row -> Either DbError (List Row)
filterRows _ _ [] = Right []
filterRows index wanted (row :: rest) = do
  value <- case at index row.values of
    Nothing => Left (InternalError "row is shorter than its table schema")
    Just value => Right value
  later <- filterRows index wanted rest
  pure (if sqlEquals value wanted then row :: later else later)

projectRows : List Nat -> List Row -> Either DbError (List Row)
projectRows _ [] = Right []
projectRows indices (row :: rest) = do
  values <- pick indices row.values
  later <- projectRows indices rest
  pure (MkRow row.rowId values :: later)

executeInstruction : Instruction -> Machine -> Either DbError Machine
executeInstruction (CreateTableOp name columns) machine = do
  database <- createTable name columns machine.database
  pure ({ database := database } machine)
executeInstruction (InsertOp name values) machine = do
  database <- insertValues name values machine.database
  pure ({ database := database } machine)
executeInstruction (OpenReadOp name) machine =
  case findTable name machine.database of
    Nothing => Left (NoSuchTable name)
    Just table => Right
      ({ activeColumns := table.columns
       , activeRows := tableRows table
       } machine)
executeInstruction (FilterEqualsOp name wanted) machine =
  case position name machine.activeColumns of
    Nothing => Left (NoSuchColumn name)
    Just index => do
      rows <- filterRows index wanted machine.activeRows
      pure ({ activeRows := rows } machine)
executeInstruction (ProjectOp AllColumns) machine = Right machine
executeInstruction (ProjectOp (NamedColumns names)) machine = do
  indices <- positions names machine.activeColumns
  columns <- pick indices machine.activeColumns
  rows <- projectRows indices machine.activeRows
  pure ({ activeColumns := columns, activeRows := rows } machine)
executeInstruction ResultRowsOp machine =
  let result = MkResultSet (columnNames machine.activeColumns)
        (map values machine.activeRows)
   in Right ({ output := Just result } machine)
executeInstruction HaltOp machine = Right ({ halted := True } machine)

step : Instruction -> Machine -> Either DbError Machine
step instruction machine =
  if machine.halted
    then Right machine
    else executeInstruction instruction machine

run : List Instruction -> Machine -> Either DbError Machine
run [] machine = Right machine
run (instruction :: rest) machine = do
  later <- step instruction machine
  if later.halted then Right later else run rest later

public export
runProgram : List Instruction -> Database -> Either DbError (Database, Maybe ResultSet)
runProgram instructions database = do
  machine <- run instructions (initialMachine database)
  pure (machine.database, machine.output)
