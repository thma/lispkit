module LispkitParser
    ( readSExpr
    , SExpr(..)
    , toString
    ) where

import Control.Monad
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as T
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT)
import Text.Parsec.Language

-- |Language definition for Scheme (taken from HUSK Scheme)
lispDef :: T.LanguageDef ()
lispDef 
  = emptyDef    
  { T.commentStart   = "#|"
  , T.commentEnd     = "|#"
  , T.commentLine    = ";"
  , T.nestedComments = True
  , T.identStart     = letter <|> symbol
  , T.identLetter    = letter <|> digit <|> symbol
  , T.reservedNames  = []
  , T.caseSensitive  = True
  }

lexer :: T.GenTokenParser String () Identity
lexer = T.makeTokenParser lispDef

dot :: ParsecT String () Identity String
dot = T.dot lexer

parens :: ParsecT String () Identity a -> ParsecT String () Identity a
parens = T.parens lexer

brackets :: ParsecT String () Identity a -> ParsecT String () Identity a
brackets = T.brackets lexer

identifier :: ParsecT String () Identity String
identifier = T.identifier lexer

whiteSpace :: ParsecT String () Identity ()
whiteSpace = T.whiteSpace lexer

lexeme :: ParsecT String () Identity a -> ParsecT String () Identity a
lexeme = T.lexeme lexer

data SExpr = SAtom String
           | SList [SExpr]
           | SInt Integer
           | SBool Bool
           deriving (Show, Eq)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~."

parseAtom :: Parser SExpr
parseAtom = do 
  atom <- identifier
  if atom == "."
    then pzero -- Do not match this form
    else 
      return $ case atom of
        "true"  -> SBool True
        "false" -> SBool False
        "True"  -> SBool True
        "False" -> SBool False
        _       -> SAtom atom

parseNumber :: Parser SExpr
parseNumber = do
  sign   <- many (oneOf "-")
  digits <- many1 digit
  case length sign of
    0 -> return $ SInt (read digits)
    1 -> return $ SInt (read $ "-" ++ digits)

parseDecimalNumber :: Parser SExpr
parseDecimalNumber = do
  _ <- try (many (string "#d"))
  sign <- many (oneOf "-")
  num <- many1 digit
  if length sign > 1
     then pzero
     else return $ (SInt . read) $ sign ++ num


parseSList :: Parser SExpr
parseSList = SList <$> sepBy parseExpr whiteSpace

parseQuoted :: Parser SExpr
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ SList [SAtom "quote" , x]

parseExpr :: Parser SExpr
parseExpr = 
      try (lexeme parseNumber)
  <|> try parseAtom
  <|> parseQuoted
  <|> try (parens parseSList)

-- | Parse an s-expression
readSExpr :: String -> Either ParseError SExpr
readSExpr = parse parseExpr "SExpr"

toString :: SExpr -> String
toString (SAtom str) = str
toString (SInt i)    = show i
toString (SBool bool) =  if bool then "true" else "false"
toString (SList list) = "(" ++ render list ++ ")"
  where
    render [] = ""
    render [hd] = toString hd
    render (hd:tl) = toString hd ++ " " ++ render tl