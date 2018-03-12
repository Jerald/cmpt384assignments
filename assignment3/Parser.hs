--Parses tokenized input stream

--Requirements:
--Uses a token stream to produce haskell representation of SLisp
--Should use SLisp data types defined elsewhere and parse into said types
--Most likely needs one function per said type

module Parser where

import Types

parseSExpression    :: [Token] -> (Maybe SExpression, [Token])
-- parseSmLispExpr     :: [Token] -> (Maybe SmLispExpr, [Token])
-- parseSmLispProgram  :: [Token] -> (Maybe SmLispProgram, [Token])

parseSExpression [] = (Nothing, [])

parseSExpression ((NumToken t):tx) = (Just (NumAtom t), tx)

parseSExpression ((AlphaNumToken t):tx) = (Just (SymAtom t), tx)
parseSExpression ((SpecialToken t):tx) = (Just (SymAtom t), tx)

parseSExpression (LParen:tx)
    | (head tx) == RParen   = (Just (List []), tail tx) -- We may just be able to get rid of this
    | otherwise             = extendList (Just (List [])) tx

parseSExpression tx = (Nothing, tx)

-- Once we've entered a list, we need to keep going until the end
extendList :: Maybe SExpression -> [Token] -> (Maybe SExpression, [Token])
extendList (Just list) (RParen:tx) = (Just list, tx)
extendList (Just (List list)) tx = 
    case parseSExpression(tx) of
        (Just sexpr, more)  -> extendList (Just (List (list ++ sexpr:[]))) more
        (Nothing, more) -> (Nothing, more)