
-- For parsing our token stream
-- Proposed BNF Grammar:
-- <statement>  ::= <atom> | "(" <statement> ")" | <statement> <connector> <statement> | ~<statement>
-- <atom>       ::= P | Q | R |...
-- <connector>  ::= ^ | v | -> | ==

module Parser where

import Data.List
import Types
import Tokenizer

--Takes a stream of tokens and separates into hypothesis tokens and goal tokens
splitHypsAndGoals :: [Token] -> ([Token], [Token]) -> ([Token], [Token])
splitHypsAndGoals (fst:rest) (hyps,goals)
    | fst /= Turnstile && rest == [] = (hyps++[fst], goals)
    | fst == Turnstile && rest == [] = (hyps, rest)
    | fst /= Turnstile               = splitHypsAndGoals rest (hyps++[fst], goals)
    | otherwise                      = (hyps, rest)

--Takes a stream of tokens and separates into individual propositional formulas
splitCommas :: [Token] -> [Token] -> [[Token]] -> [[Token]]
splitCommas (fst:rest) temp resTemp
    | fst /= Comma && rest /= [] = splitCommas rest (temp++[fst]) resTemp
    | fst == Comma && rest /= [] = splitCommas rest [] (resTemp ++ [temp])
    | fst /= Comma && rest == [] = resTemp ++ [temp++[fst]]
    | fst == Comma && rest == [] = resTemp ++ [temp]

parseTokens :: [Token] -> ProofConjecture
parseTokens [] = Conjecture [] []
parseTokens tokens = 
    let splitted = splitHypsAndGoals tokens ([],[]) 
    in Conjecture (map parseMain (splitCommas (fst splitted) [] [])) (map parseMain (splitCommas (snd splitted) [] []))

parseMain :: [Token] -> PF
parseMain tokens = 
    case parseStatement tokens of
        (res, []) -> res
        _         -> error("Parse Fail")

parseStatement :: [Token] -> (PF, [Token])
parseStatement (Letter x:rest) = (Prop x, rest)
parseStatement (LBrack:rest) = 
    case parseStatement rest of
        (res, RBrack:even_more) -> (Group res, even_more)
        _                       -> error("Parse Fail")
parseStatement (Tilde:rest) =
    case parseStatement rest of
        (res, even_more) -> (Neg res, even_more)
--        _                -> error("Parse Fail")
parseStatement tokens = 
    case parseStatement tokens of
        (res, Arrow:rest) ->
            case parseStatement(rest) of
                (res1, even_more) -> (Imp res res1, even_more)
--                _                 -> error("Parse Fail")

        (res, Hat:rest)   ->
            case parseStatement(rest) of
                (res1, even_more) -> (Conj res res1, even_more)
--                _                 -> error("Parse Fail")

        (res, Vee:rest)   ->
            case parseStatement(rest) of
                (res1, even_more) -> (Disj res res1, even_more)
--                _                 -> error("Parse Fail")

        (res, Equal:rest) ->
            case parseStatement(rest) of
                (res1, even_more) -> (Equiv res res1, even_more)
--                _                 -> error("Parse Fail")

        _                 -> error("Parse Fail")
