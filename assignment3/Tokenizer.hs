--Tokenizes SLisp code

--Requirements:
--Read SLisp from a file and produce a token stream
--Ensure the whole file is read
--Use elsewhere defined token type(s) for haskell representation

import System.Environment
import System.IO
import Data.Maybe
import Data.Char
import Types
parseString :: [Char] -> Maybe (Token, [Char])
parseMain :: [Char] -> Maybe [Token]


parseAlphaNum :: [Char] -> [Char]
parseSymbol :: [Char] -> [Char]
parseNumeral :: [Char] -> [Char]

isSpeSymbol :: Char -> Bool
isSpeSymbol(a)
  | a == '+' || a == '-' || a == '*' || a == '/' || a == '<' || a == '>' || a == '&' || a == '|' || a == '@' = True
  | a == '#' || a == '$' || a == '%' || a == '?' || a == ':' = True
  | otherwise = False

parseSymbol [] = ""
parseSymbol (a:rest)
  | isSpeSymbol(a) = [a] ++ parseSymbol(rest)
  | otherwise   = ""

parseNumeral [] = ""
parseNumeral (a:rest)
  | isDigit(a) = [a] ++ parseNumeral(rest)
  | otherwise = ""

parseAlphaNum [] = ""
parseAlphaNum (a:rest)
  | isDigit(a) || isLetter(a) = [a] ++ parseAlphaNum(rest)
  | otherwise = ""

parseString []= Nothing
parseString ('-':rest)
  | isDigit(head rest)  = Just (NumToken (negate (read (parseNumeral(rest)) :: Int)), (drop (length (parseNumeral(rest))) rest))
  | head rest == '-' && rest !! 1 == '>' = Just (Arrow, drop 2 rest)
  | otherwise = Just (SpecialToken ("-" ++ (parseSymbol(rest))), (drop (length (parseSymbol(rest))) rest))


parseString (a:rest)
  | a == ';' = Just (Comment rest, "")
  | a == '(' = Just (LBrak,rest)
  | a == ')' = Just (RBrak,rest)
  | a == '[' = Just (LBrace,rest)
  | a == ']' = Just (RBrace,rest)
  | a == '{' = Just (LParen,rest)
  | a == '}' = Just (RParen,rest)
  | a == '=' = Just (Equal,rest)
  | a == '"' = Just (Quote,rest)
  | a == ':' = Just (Colon,rest)
  | isSpeSymbol(a)  = Just (SpecialToken ([a]++(parseSymbol(rest))) , (drop (length (parseSymbol(rest))) rest))
  | isDigit(a)      = Just (NumToken (read (parseNumeral([a] ++ rest)) :: Int), (drop (length (parseNumeral(rest))) rest))
  | isLetter(a)     = Just (AlphaNumToken ([a]++(parseAlphaNum(rest))) , (drop (length (parseAlphaNum(rest))) rest))
parseToken :: [Char] -> Maybe [Token]
parseToken s = do
  (tok1,rest) <- parseString(s)
  return ([tok1] ++ (fromMaybe [] (parseToken(rest))))

parseMain s = parseToken(s)
--  return ([tok1]++tok2
