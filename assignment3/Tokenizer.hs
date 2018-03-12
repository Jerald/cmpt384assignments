--Tokenizes SLisp code

--Requirements:
--Read SLisp from a file and produce a token stream
--Ensure the whole file is read
--Use elsewhere defined token type(s) for haskell representation

import System.Environment
import System.IO

import Types
parseString :: [Char] -> Maybe (Token, [Char])
parseNumeral :: [Char] -> [Char]
parseSymbol :: [Char] -> [Char]

isInt :: Char -> Bool
isInt(a)
  | a == '1' || a == '2' || a == '3' || a == '4' || a == '5' = True
  | a == '6' || a == '7' || a == '8' || a == '9' || a == '0' = True
  | otherwise = False

isSymbol :: Char -> Bool
isSymbol(a)
  | a == '+' || a == '-' || a == '*' || a == '/' || a == '<' || a == '>' || a == '&' || a == '|' || a == '@' = True
  | a == '#' || a == '$' || a == '%' || a == '?' || a == ':' = True
  | otherwise = False

parseSymbol [] = ""
parseSymbol (a:rest)
  | isSymbol(a) = [a] ++ parseSymbol(rest)
  | otherwise   = ""

parseNumeral [] = ""
parseNumeral (a:rest)
  | isInt(a) = [a] ++ parseNumeral(rest)
  | otherwise = ""

parseString ('-':rest)
  | isInt(head rest)  = Just (NumToken (negate (read (parseNumeral(rest)) :: Int)), (drop (length (parseNumeral(rest))) rest))
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
  | isSymbol(a) = Just (SpecialToken ([a]++(parseSymbol(rest))) , (drop (length (parseSymbol(rest))) rest))

