module SQLite.Compiler

import SQLite.SQL
import SQLite.VDBE

%default total

predicateCode : Maybe Predicate -> List Instruction
predicateCode Nothing = []
predicateCode (Just (ColumnEquals name value)) = [FilterEqualsOp name value]

public export
compile : Statement -> List Instruction
compile (CreateTable name columns) =
  [CreateTableOp name columns, HaltOp]
compile (InsertValues name values) =
  [InsertOp name values, HaltOp]
compile (SelectRows projection name predicate) =
  [OpenReadOp name]
    ++ predicateCode predicate
    ++ [ProjectOp projection, ResultRowsOp, HaltOp]
