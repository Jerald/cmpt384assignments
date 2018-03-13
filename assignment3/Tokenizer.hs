--Tokenizes SLisp code

--Requirements:
--Read SLisp from a file and produce a token stream
--Ensure the whole file is read
--Use elsewhere defined token type(s) for haskell representation

module Tokenizer where


import Data.Maybe
import Data.Char
import Control.Exception
import Types



tokenizeString :: [Char] -> Maybe ([Token], [Char])
tokenizeMain :: [Char] -> Maybe [Token]


tokenizeAlphaNum :: [Char] -> [Char]
tokenizeSymbol :: [Char] -> [Char]
tokenizeNumeral :: [Char] -> [Char]

isSpeSymbol :: Char -> Bool
isSpeSymbol(a)
  | a == '+' || a == '-' || a == '*' || a == '/' || a == '<' || a == '>' || a == '&' || a == '|' || a == '@' = True
  | a == '#' || a == '$' || a == '%' || a == '?' || a == ':' = True
  | otherwise = False

tokenizeSymbol [] = ""
tokenizeSymbol (a:rest)
  | isSpeSymbol(a) = [a] ++ tokenizeSymbol(rest)
  | otherwise   = ""

tokenizeNumeral [] = ""
tokenizeNumeral (a:rest)
  | isDigit(a) = [a] ++ tokenizeNumeral(rest)
  | otherwise = ""

tokenizeAlphaNum (a:b:rest)
  | isDigit(a) || isLetter(a)  = [a] ++ tokenizeAlphaNum([b]++rest)
  | a == '-'  && (isDigit(b) || isLetter(b)) = [a]++[b]++ tokenizeAlphaNum(rest)
  | otherwise = ""

tokenizeAlphaNum (a:b)
  | isDigit(a) || isLetter(a)  = [a] ++ tokenizeAlphaNum(b)
  | a == '-'  && (isDigit(head b) || isLetter(head b)) = [a]++b
  | otherwise = ""

tokenizeAlphaNum(a)
  | isDigit(head a) || isLetter(head a) = a
  | otherwise = ""

tokenizeComment [] = ""
tokenizeComment (a:rest)
  | a == '\n' = ""
  | otherwise = [a] ++ (tokenizeComment rest)

tokenizeString [] = Nothing
tokenizeString ('-':rest)
  | isDigit(head rest) = Just ([NumToken (negate (read (tokenizeNumeral(rest)) :: Int))], (drop (length (tokenizeNumeral(rest))) rest))
  | head rest == '-' && rest !! 1 == '>' = Just ([Arrow], drop 2 rest)
  | otherwise = Just ([SpecialToken ("-" ++ (tokenizeSymbol(rest)))], (drop (length (tokenizeSymbol(rest))) rest))

tokenizeString ('\n':rest)
  | length rest < 3 = Just ([],rest)
  | (head rest) == ';' && rest !! 1 == ';' && rest !! 2 == ';' && rest !! 3 /= '\n' = Just ([Comment (drop 3 (tokenizeComment(rest)))], (drop (length (tokenizeComment(rest))) rest))
  | otherwise  = Just ([], rest)

tokenizeString (a:rest)
  | a == '(' = Just ([LParen],rest)
  | a == ')' = Just ([RParen],rest)
  | a == '[' = Just ([LBrack],rest)
  | a == ']' = Just ([RBrack],rest)
  | a == '{' = Just ([LBrace],rest)
  | a == '}' = Just ([RBrace],rest)
  | a == '=' = Just ([Equal],rest)
  | a == '"' = Just ([Quote],rest)
  | a == ':' = Just ([Colon],rest)
  | a == ';' = Just ([Semicolon],rest)
  | isSpeSymbol(a)  = Just ([SpecialToken ([a]++(tokenizeSymbol(rest)))] , (drop (length (tokenizeSymbol(rest))) rest))
  | isDigit(a)      = Just ([NumToken (read (tokenizeNumeral([a] ++ rest)) :: Int)], (drop (length (tokenizeNumeral(rest))) rest))
  | isLetter(a)     = Just ([AlphaNumToken ([a]++(tokenizeAlphaNum(rest)))] , (drop (length (tokenizeAlphaNum(rest))) rest))
  | otherwise       = error("urmum")

tokenizeToken :: [Char] -> Maybe [Token]
tokenizeToken s = do
  (tok1,rest) <- tokenizeString(s)
  return (tok1 ++ (fromMaybe [] (tokenizeToken(rest))))

tokenizeMain s = tokenizeToken(s)

--tokenizeFile :: [Char] -> Maybe [Token]
