module LispkitParser
    ( readSExpr
    , SExpr(..)
    , toString
    ) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec as P

data SExpr = SAtom String
           | SList [SExpr]
           | SInt Integer
           | SBool Bool
           deriving (Show, Eq)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser SExpr
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = first:rest
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
    --SInt . read <$> many1 digit

parseSList :: Parser SExpr
parseSList = SList <$> sepBy parseExpr spaces

parseQuoted :: Parser SExpr
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ SList [SAtom "quote" , x]

parseExpr :: Parser SExpr
parseExpr = parseAtom
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseSList
               char ')'
               return x

lexeme :: Parser a -> Parser a
lexeme parser = parser <* P.spaces

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