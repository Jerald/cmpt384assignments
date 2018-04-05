
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

--Takes a stream of tokens and separates into individual propositional formulaes
splitCommas :: [Token] -> [Token] -> [[Token]] -> [[Token]]
splitCommas (fst:rest) temp resTemp
    | fst /= Comma && rest /= [] = splitCommas rest (temp++[fst]) resTemp
    | fst == Comma && rest /= [] = splitCommas rest [] (resTemp ++ [temp])
    | fst /= Comma && rest == [] = resTemp ++ [temp++[fst]]
    | fst == Comma && rest == [] = resTemp ++ [temp]

makeConjecture :: [Token] -> ProofConjecture
makeConjecture [] = Conjecture [] []
makeConjecture tokens = 
    let splitted = splitHypsAndGoals tokens ([],[]) 
    in Conjecture (map parseMain (splitCommas (fst splitted) [] [])) (map parseMain (splitCommas (snd splitted) [] []))

parseMain :: [Token] -> PF
parseMain tokens = 
    case parseStatement tokens of
        (res, []) -> res
        _         -> error("Parse Fail: token stream is not empty")

parseStatement :: [Token] -> (PF, [Token])
parseAtom :: [Token] -> (PF, [Token])

parseStatement (LBrack:rest) = 
    case parseStatement rest of
        (res, RBrack:even_more) -> (Group res, even_more)

parseStatement (Tilde:LBrack:rest) = 
    case parseStatement rest of
        (res, RBrack:even_more) -> (Neg (Group res), even_more)

parseStatement tokens = 
    case parseAtom tokens of
        (res, Arrow:rest) ->
            case parseStatement(rest) of
                (res1, even_more) -> (Imp res res1, even_more)

        (res, Hat:rest)   ->
            case parseStatement(rest) of
                (res1, even_more) -> (Conj res res1, even_more)

        (res, Vee:rest)   ->
            case parseStatement(rest) of
                (res1, even_more) -> (Disj res res1, even_more)

        (res, Equal:rest) ->
            case parseStatement(rest) of
                (res1, even_more) -> (Equiv res res1, even_more)

        (res, rest)       -> (res, rest)

parseAtom (Letter x:rest) = (Prop x, rest)
parseAtom (Tilde:Letter x:rest) = (Neg(Prop x), rest)
