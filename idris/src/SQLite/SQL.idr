module SQLite.SQL

import SQLite.Value

%default covering

public export
data Token
  = WordToken String
  | IntegerToken String
  | TextToken String
  | LeftParenToken
  | RightParenToken
  | CommaToken
  | StarToken
  | EqualsToken
  | SemicolonToken

public export
Eq Token where
  WordToken left == WordToken right = left == right
  IntegerToken left == IntegerToken right = left == right
  TextToken left == TextToken right = left == right
  LeftParenToken == LeftParenToken = True
  RightParenToken == RightParenToken = True
  CommaToken == CommaToken = True
  StarToken == StarToken = True
  EqualsToken == EqualsToken = True
  SemicolonToken == SemicolonToken = True
  _ == _ = False

public export
Show Token where
  show (WordToken word) = word
  show (IntegerToken integer) = integer
  show (TextToken text) = "'" ++ text ++ "'"
  show LeftParenToken = "("
  show RightParenToken = ")"
  show CommaToken = ","
  show StarToken = "*"
  show EqualsToken = "="
  show SemicolonToken = ";"

public export
data Projection
  = AllColumns
  | NamedColumns (List String)

public export
Eq Projection where
  AllColumns == AllColumns = True
  NamedColumns left == NamedColumns right = left == right
  _ == _ = False

public export
data Predicate = ColumnEquals String SqlValue

public export
Eq Predicate where
  ColumnEquals leftName leftValue == ColumnEquals rightName rightValue =
    leftName == rightName && leftValue == rightValue

public export
data Statement
  = CreateTable String (List Column)
  | InsertValues String (List SqlValue)
  | SelectRows Projection String (Maybe Predicate)

public export
Eq Statement where
  CreateTable leftName leftColumns == CreateTable rightName rightColumns =
    leftName == rightName && leftColumns == rightColumns
  InsertValues leftName leftValues == InsertValues rightName rightValues =
    leftName == rightName && leftValues == rightValues
  SelectRows leftProjection leftName leftPredicate
    == SelectRows rightProjection rightName rightPredicate =
      leftProjection == rightProjection
        && leftName == rightName
        && leftPredicate == rightPredicate
  _ == _ = False

asciiUpper : Char -> Char
asciiUpper character =
  if character >= 'a' && character <= 'z'
    then chr (ord character - 32)
    else character

canonical : String -> String
canonical = pack . map asciiUpper . unpack

public export
sameName : String -> String -> Bool
sameName left right = canonical left == canonical right

keyword : String -> String -> Bool
keyword expected actual = expected == canonical actual

white : Char -> Bool
white character =
  character == ' '
    || character == '\n'
    || character == '\r'
    || character == '\t'

digit : Char -> Bool
digit character = character >= '0' && character <= '9'

nameStart : Char -> Bool
nameStart character =
  (character >= 'a' && character <= 'z')
    || (character >= 'A' && character <= 'Z')
    || character == '_'

namePart : Char -> Bool
namePart character = nameStart character || digit character

takeWhileChars : (Char -> Bool) -> List Char -> (List Char, List Char)
takeWhileChars accepts [] = ([], [])
takeWhileChars accepts (character :: rest) =
  if accepts character
    then
      let (front, back) = takeWhileChars accepts rest
       in (character :: front, back)
    else ([], character :: rest)

quoted : List Char -> List Char -> Either String (String, List Char)
quoted accumulated [] = Left "unterminated SQL string literal"
quoted accumulated ('\'' :: '\'' :: rest) = quoted ('\'' :: accumulated) rest
quoted accumulated ('\'' :: rest) = Right (pack (reverse accumulated), rest)
quoted accumulated (character :: rest) = quoted (character :: accumulated) rest

lexChars : List Char -> Either String (List Token)
lexChars [] = Right []
lexChars (character :: rest) =
  if white character
    then lexChars rest
    else case character of
      '(' => add LeftParenToken rest
      ')' => add RightParenToken rest
      ',' => add CommaToken rest
      '*' => add StarToken rest
      '=' => add EqualsToken rest
      ';' => add SemicolonToken rest
      '\'' => do
        (text, later) <- quoted [] rest
        tokens <- lexChars later
        pure (TextToken text :: tokens)
      '-' =>
        case rest of
          next :: _ =>
            if digit next
              then lexNumber [character] rest
              else Left "'-' is only supported as the sign of an integer literal"
          [] => Left "SQL cannot end with '-'"
      _ =>
        if digit character
          then lexNumber [] (character :: rest)
          else if nameStart character
            then lexWord (character :: rest)
            else Left ("unsupported SQL character " ++ show character)
  where
    add : Token -> List Char -> Either String (List Token)
    add token later = do
      tokens <- lexChars later
      pure (token :: tokens)

    lexNumber : List Char -> List Char -> Either String (List Token)
    lexNumber leading characters =
      let (digits, later) = takeWhileChars digit characters
          token = IntegerToken (pack (leading ++ digits))
       in do
         tokens <- lexChars later
         pure (token :: tokens)

    lexWord : List Char -> Either String (List Token)
    lexWord characters =
      let (name, later) = takeWhileChars namePart characters
       in do
         tokens <- lexChars later
         pure (WordToken (pack name) :: tokens)

public export
tokenize : String -> Either String (List Token)
tokenize = lexChars . unpack

digitValue : Char -> Integer
digitValue character = cast (ord character - ord '0')

positiveInteger : List Char -> Maybe Integer
positiveInteger [] = Nothing
positiveInteger characters = foldl step (Just 0) characters
  where
    step : Maybe Integer -> Char -> Maybe Integer
    step Nothing _ = Nothing
    step (Just value) character =
      if digit character
        then Just (value * 10 + digitValue character)
        else Nothing

integerLiteral : String -> Maybe Integer
integerLiteral text =
  case unpack text of
    '-' :: digits => map negate (positiveInteger digits)
    digits => positiveInteger digits

literal : Token -> Either String SqlValue
literal (TextToken text) = Right (SqlText text)
literal (IntegerToken text) =
  case integerLiteral text of
    Just value => Right (SqlInteger value)
    Nothing => Left ("invalid integer literal " ++ text)
literal (WordToken word) =
  if keyword "NULL" word
    then Right SqlNull
    else Left ("expected a literal, found " ++ word)
literal token = Left ("expected a literal, found " ++ show token)

affinity : String -> Either String Affinity
affinity word =
  if keyword "INTEGER" word
    then Right IntegerAffinity
    else if keyword "REAL" word
      then Right RealAffinity
      else if keyword "TEXT" word
        then Right TextAffinity
        else if keyword "BLOB" word
          then Right BlobAffinity
          else Left ("unsupported column affinity " ++ word)

finish : List Token -> Either String ()
finish [] = Right ()
finish [SemicolonToken] = Right ()
finish tokens = Left ("unexpected trailing SQL tokens " ++ show tokens)

parseColumns : List Token -> Either String (List Column, List Token)
parseColumns (WordToken columnName :: WordToken typeName :: rest) = do
  columnAffinity <- affinity typeName
  let column = MkColumn columnName columnAffinity
  case rest of
    CommaToken :: later => do
      (columns, remaining) <- parseColumns later
      pure (column :: columns, remaining)
    RightParenToken :: later => Right ([column], later)
    tokens => Left ("expected ',' or ')' after column, found " ++ show tokens)
parseColumns tokens = Left ("expected a column name and affinity, found " ++ show tokens)

parseValues : List Token -> Either String (List SqlValue, List Token)
parseValues (token :: rest) = do
  value <- literal token
  case rest of
    CommaToken :: later => do
      (values, remaining) <- parseValues later
      pure (value :: values, remaining)
    RightParenToken :: later => Right ([value], later)
    tokens => Left ("expected ',' or ')' after value, found " ++ show tokens)
parseValues [] = Left "expected a value"

parseProjection : List Token -> Either String (Projection, String, List Token)
parseProjection (StarToken :: WordToken from :: WordToken tableName :: rest) =
  if keyword "FROM" from
    then Right (AllColumns, tableName, rest)
    else Left ("expected FROM, found " ++ from)
parseProjection tokens = named [] tokens
  where
    named : List String -> List Token -> Either String (Projection, String, List Token)
    named accumulated (WordToken name :: CommaToken :: rest) =
      named (name :: accumulated) rest
    named accumulated (WordToken name :: WordToken from :: WordToken tableName :: rest) =
      if keyword "FROM" from
        then Right (NamedColumns (reverse (name :: accumulated)), tableName, rest)
        else Left ("expected ',' or FROM, found " ++ from)
    named _ rest = Left ("invalid SELECT projection near " ++ show rest)

parsePredicate : List Token -> Either String (Maybe Predicate, List Token)
parsePredicate (WordToken whereWord :: WordToken columnName :: EqualsToken :: token :: rest) =
  if keyword "WHERE" whereWord
    then do
      value <- literal token
      pure (Just (ColumnEquals columnName value), rest)
    else Right (Nothing, WordToken whereWord :: WordToken columnName :: EqualsToken :: token :: rest)
parsePredicate tokens = Right (Nothing, tokens)

parseNonCreate : List Token -> Either String Statement
parseSelect : List Token -> Either String Statement

public export
parse : List Token -> Either String Statement
parse (WordToken create :: WordToken table :: WordToken tableName :: LeftParenToken :: rest) =
  if keyword "CREATE" create && keyword "TABLE" table
    then do
      (columns, remaining) <- parseColumns rest
      finish remaining
      pure (CreateTable tableName columns)
    else parseNonCreate (WordToken create :: WordToken table :: WordToken tableName :: LeftParenToken :: rest)
parse tokens = parseNonCreate tokens

parseNonCreate (WordToken insert :: WordToken into :: WordToken tableName
  :: WordToken values :: LeftParenToken :: rest) =
    if keyword "INSERT" insert && keyword "INTO" into && keyword "VALUES" values
      then do
        (row, remaining) <- parseValues rest
        finish remaining
        pure (InsertValues tableName row)
      else parseSelect (WordToken insert :: WordToken into :: WordToken tableName
        :: WordToken values :: LeftParenToken :: rest)
parseNonCreate tokens = parseSelect tokens

parseSelect (WordToken select :: rest) =
  if keyword "SELECT" select
    then do
      (projection, tableName, afterTable) <- parseProjection rest
      (predicate, remaining) <- parsePredicate afterTable
      finish remaining
      pure (SelectRows projection tableName predicate)
    else Left ("unsupported SQL statement beginning with " ++ select)
parseSelect [] = Left "empty SQL statement"
parseSelect tokens = Left ("unsupported SQL statement " ++ show tokens)
