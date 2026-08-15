module Main

import SQLite.ReadOnly
import SQLite.VDBE
import System

%default covering

usage : String
usage = "usage: sqlite-idris DATABASE \"SELECT ...\""

main : IO ()
main = do
  arguments <- getArgs
  case arguments of
    [_ , path, sql] => do
      answer <- queryFile path sql
      case answer of
        Left error => die error
        Right result => putStr (show result)
    _ => die usage
