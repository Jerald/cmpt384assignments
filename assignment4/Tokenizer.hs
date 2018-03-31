-- For tokenizing our input stream

module Tokenizer where

import Types
import Data.Char

-- Main entry point for tokenizeation
tokenize :: [Char] -> [Token]
tokenize input =
    case tokenizeMain ([], input) of
        (tokens, []) -> tokens
        _            -> error("Tokenization failure! :(")

-- Because I'm lazy
tokOut :: [Token] -> [Char] -> Token -> ([Token], [Char])
tokOut tokens input tok = tokenizeMain (tokens ++ (tok:[]), input)

-- Takes in input stream and returns a token stream
-- Invalid characters are stripped
tokenizeMain :: ([Token], [Char]) -> ([Token], [Char])

-- Base case on empty input
tokenizeMain (tokens, []) = (tokens, [])
tokenizeMain (tokens, c:[])
    | c == ' '  = (tokens, [])
    | otherwise = tokenizeMain (tokens, (c:' ':[]))
-- c is first token, cs is second token, cx is second+ tokens, csx is third+ tokens
tokenizeMain (tokens, (c:cs:csx))
    | c == 'v'              = tokOut tokens cx Vee -- Before letter, because 'v' would catch as one
    | isLetter(c)           = tokOut tokens cx (Letter c)
    | c == '-' && cs == '>' = tokOut tokens csx Arrow
    | c == '^'              = tokOut tokens cx Hat
    | c == '~'              = tokOut tokens cx Tilde
    | c == '=' && cs == '=' = tokOut tokens csx Equal
    | c == '('              = tokOut tokens cx LBrack
    | c == ')'              = tokOut tokens cx RBrack
    | c == '|' && cs == '-' = tokOut tokens csx Turnstile
    | otherwise             = tokenizeMain (tokens, cx)  -- Skip the invalid token
    where cx = (cs:csx)