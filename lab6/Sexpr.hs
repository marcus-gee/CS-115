----------------------------------------------------------------------
-- S-expression parser.
----------------------------------------------------------------------

module Sexpr where

import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

data Atom =
    BoolA   Bool
  | IntA    Integer
  | FloatA  Double
  | IdA     String  -- identifier
  | StringA String
  deriving (Show)

data Sexpr =
    AtomS Atom
  | ListS [Sexpr]

  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"

-- helper for parseFloat that deals with exponents
parseFloatHelper :: Parser String
parseFloatHelper = do
    exp  <- oneOf "eE"
    sign <- option "" (string "-" <|> string "+")
    nums <- many1 digit
    return ([exp] ++ sign ++ nums)
    
parseFloat :: Parser Double
parseFloat = do
  sign <- option "" (string "-")
  digits <- many1 digit
  char '.'
  f <- many1 digit
  e <- option "" parseFloatHelper
  return (read (sign ++ digits ++ "." ++ f ++ e) :: Double)
  <?> "floating-point number"

parseString :: Parser String
parseString = do
    char '\"'
    s <- many (noneOf "\"")
    char '\"'
    return s

parseId :: Parser String
parseId = many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> try (parseString >>= return . StringA)
  <|> (parseId >>= return . IdA)
  <?> "atom"

parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"

-- helper function for parseList
parseListHelper :: Char -> Char -> Parser [Sexpr]
parseListHelper open close = do
  char open
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  char close
  return ss
  <?> "helper function for parseList"

-- Parse a list of S-expressions, delimited by parentheses,
-- separated by whitespace/comments.  
parseList :: Parser [Sexpr]
parseList = 
  parseListHelper '(' ')'
  <|> parseListHelper '[' ']'
  <|> parseListHelper '{' '}'
  <?> "list of S-expressions"

-- C.3
{- 
 - Our parser uses alternation if one of the parsers fails, then we just try the
 - the other parser, and we won't need to backtrack, so the 'try' combinator
 - is not needed.
 -}

parseQuote :: Parser Sexpr
parseQuote = do 
    char '\''
    q <- parseSexpr
    return (ListS [AtomS (IdA "quote"), q])
    
-- Parse a single S-expressions.
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseList >>= return . ListS)
  <|> parseQuote
  <?> "S-expression"

-- Parse a series of Sexprs from a string representing the entire contents of a
-- file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print a Sexpr.
ppSexpr :: Int -> Sexpr -> String
ppSexpr i (AtomS a)  = indent i ++ show a
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"
-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

test :: IO ()
test = runPpSexpr "test.scm"

