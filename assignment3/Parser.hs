--Parses tokenized input stream

--Requirements:
--Uses a token stream to produce haskell representation of SLisp
--Should use SLisp data types defined elsewhere and parse into said types
--Most likely needs one function per said type


module Parser where

import System.IO
import System.Environment
import Data.Maybe
import Data.Char
import Data.Typeable

import Types
    
parseSExpression    :: [Token] -> (Maybe SExpression, [Token])
extendList          :: Maybe SExpression -> [Token] -> (Maybe SExpression, [Token])

parseSExpression [] = (Nothing, [])

parseSExpression ((NumToken t):tx) = (Just (NumAtom t), tx)

parseSExpression ((AlphaNumToken t):tx) = (Just (SymAtom t), tx)
parseSExpression ((SpecialToken t):tx) = (Just (SymAtom t), tx)

parseSExpression (LParen:t:tx)
    | t == RParen           = (Just (List []), tx) -- We may just be able to get rid of this
    | otherwise             = extendList (Just (List [])) tx

-- End case, might need to change
parseSExpression tx = (Nothing, tx)

extendList (Just list) (RParen:tx) = (Just list, tx)
extendList (Just (List list)) tx = 
    case parseSExpression(tx) of
        (Just sexpr, more)  -> extendList (Just (List (list ++ sexpr:[]))) more
        (Nothing, more)     -> (Nothing, more)

--------------------------------------------------------------------

parseIdentifier     :: [Token] -> (Maybe Identifier, [Token])
-- parseLocalDef       :: [Token] -> (Maybe LocalDef, [Token])
parseSmLispExpr     :: [Token] -> (Maybe SmLispExpr, [Token])
parseValue          :: [Token] -> (Maybe SmLispExpr, [Token])

parseIdentifier []                      = (Nothing, [])
parseIdentifier ((AlphaNumToken t):tx)  = (Just t, tx)
parseIdentifier tx                      = (Nothing, tx)


-- parseLocalDef []                        = (Nothing, [])
-- parseLocalDef -- Deal with this as a part of SmLispExpr
-- parseLocalDef tx                        = (Nothing, tx)

-- Helper function which wraps func output to SExpr
wrapValue (Just sexpr, tx) = (Just (SExpr sexpr), tx)
wrapValue (Nothing, tx) = (Nothing, tx)

parseValue []                           = (Nothing, [])


    -- parseValue ((SpecialToken t):tx) = -- Not sure how to cover these special cases...
parseValue tx = wrapValue (parseSExpression tx)

-- matchWrap :: a -> a -> bool

parseSmLispExpr [] = (Nothing, [])

-- Matching numeric atoms in SExpressions
parseSmLispExpr (num@(NumToken t):tx) =
    typeWrap SExpr (parseSExpression (num:tx))

-- Matching function pattern for FnCall
parseSmLispExpr ((AlphaNumToken ident):LBrack:tx) =
    case parseSmLispExpr tx of
        (Just expr, more)   -> extendFuncArgs more ident (expr:[])
        (Nothing, more)     -> (Nothing, more)

-- Matching Identifiers to Variables
parseSmLispExpr (alphtok@(AlphaNumToken t):tx) =
    typeWrap Variable (parseIdentifier (alphtok:tx))

-- Matching AlphaNumeric symbolic atoms in SExpressions to SExpr
parseSmLispExpr (Quote:alphtok@(AlphaNumToken t):Quote:tx) = 
    typeWrap SExpr (parseSExpression (alphtok:tx))

-- Matching SpecialSymbol symbolic atoms in SExpressions to SExpr
parseSmLispExpr (Quote:spectok@(SpecialToken t):Quote:tx) =
    typeWrap SExpr (parseSExpression (spectok:tx))

-- Matching LParens for beginnings of lists in SExpressions to SExpr
parseSmLispExpr (LParen:tx) =
    typeWrap SExpr (parseSExpression (LParen:tx))

-- Matching LBrack for beginnings of conditional expressions
parseSmLispExpr (LBrack:tx) =
    case parseCondClause tx of
        (Just clause, more) -> extendCondExpr more (CondExpr (clause:[]))
        (Nothing, more) -> (Nothing, more)

-- Matching LBrace for let expression
parseSmLispExpr (LBrace:tx) =
    case buildLocalDefs tx [] of
        (Just defs, more) -> case parseSmLispExpr more of
                                (Just expr, morer) -> (Just (LetExpr defs expr), morer)
                                (Nothing, morer) -> (Nothing, morer)
        (Nothing, more) -> (Nothing, more)

parseSmLispExpr tx =
    typeWrap SExpr (parseSExpression tx)


        
-- Doesn't use parseIdentifier, MAY cause an issue because of this
parseLocalDef :: [Token] -> (Maybe [LocalDef], [Token])
parseLocalDef ((AlphaNumToken alph):Equal:tx) = 
    case parseSmLispExpr tx of
        (Just expr, more) -> (Just ((Binding alph expr):[]), more)
        (Nothing, more) -> (Nothing, more)
parseLocalDef tx = (Nothing, tx)

buildLocalDefs :: [Token] -> [LocalDef] -> (Maybe [LocalDef], [Token])
buildLocalDefs (Colon:tx) defs = (Just defs, tx)
buildLocalDefs tx defs = 
    case parseLocalDef tx of
        (Just ndef, more) -> buildLocalDefs more (defs ++ ndef)
        (Nothing, more) -> (Nothing, more)

-- Takes the token stream, the func name, and the arg list so far
extendFuncArgs :: [Token] -> Identifier -> [SmLispExpr] -> (Maybe SmLispExpr, [Token])
extendFuncArgs (Semicolon:tx) ident args =
    case parseSmLispExpr tx of
        (Just expr, more) -> extendFuncArgs more ident (args ++ expr:[])
        (Nothing, more) -> (Nothing, more)

extendFuncArgs (RBrack:tx) ident args = (Just (FnCall ident args), tx)
extendFuncArgs tx _ _ = (Nothing, tx)



parseCondClause     :: [Token] -> (Maybe CondClause, [Token])
extendCondClause    :: [Token] -> SmLispExpr -> (Maybe CondClause, [Token])
parseCondClause tx =
    case parseSmLispExpr tx of
        (Just expr, Arrow:more) -> extendCondClause more expr
        (Nothing, more) -> (Nothing, more)

extendCondClause tx expr =
    case parseSmLispExpr tx of 
        (Just exprx, more) -> (Just (Clause expr exprx), more) 
        (Nothing, more) -> (Nothing, more)

-- Conditional expression matching


extendCondExpr :: [Token] -> SmLispExpr -> (Maybe SmLispExpr, [Token])
extendCondExpr [] _ = (Nothing, [])
extendCondExpr (Semicolon:tx) (CondExpr clauses) =
    case parseCondClause tx of
        (Just clause, more)    -> extendCondExpr more (CondExpr (clauses ++ (clause:[])))
        (Nothing, more)        -> (Nothing, more) 
extendCondExpr (RBrack:tx) (CondExpr clauses) = (Just (CondExpr clauses), tx) 





-- parseSmLispExpr tx = typeWrap SExpr (parseSExpression tx)

-- Takes a type constructor and the ouput from a parse function
typeWrap :: (a -> b) -> (Maybe a, [Token]) -> (Maybe b, [Token])
typeWrap func (Just input, tx) = (Just (func input), tx)
typeWrap func (Nothing, tx) = (Nothing, tx)

-- constructWrap :: (a -> b) -> b -> a
-- constructWrap func input =
--     case b of (out) -> out


parseSmLispProgram  :: Maybe [Token] -> (Maybe SmLispProgram, [Token])
parseDefinition     :: [Token] -> (Maybe Definition, [Token])
buildDefinition     :: [Token] -> SmLispProgram -> (Maybe SmLispProgram, [Token])

parseConstDef       :: [Token] -> Identifier -> (Maybe Definition, [Token])

parseFuncDef        :: [Token] -> Identifier -> (Maybe Definition, [Token])
extendParam         :: [Token] -> [Identifier] -> (Maybe Definition, [Token])

--Entry point for Parser.hs
parseSmLispProgram (Just tx) = buildDefinition tx []

buildDefinition tx program =
    case (parseDefinition tx) of
        (Just def, more)    -> buildDefinition more (program ++ (def:[]))
        (Nothing, more)     -> (Nothing, more)

-- All definitions start with an identifier, so this is the only pattern to match
parseDefinition ((AlphaNumToken t):tx)
    | head tx == Equal  = parseConstDef (tail tx) ident 
    | head tx == LBrack = parseFuncDef (tail tx) ident
    | otherwise         = (Nothing, ((AlphaNumToken ident):tx))
    where (Just ident, _) = parseIdentifier ((AlphaNumToken t):[])

parseDefinition tx = (Nothing, tx)

-- Single case to cover
parseConstDef tx ident =
    case (parseSmLispExpr tx) of
        (Just expr, tx) -> (Just (ConstantDef ident expr), tx)
        (Nothing, tx)   -> (Nothing, tx)

-- Requires extension due to parameter list
parseFuncDef tx ident =
    case (parseIdentifier tx) of
        (Just param, more)    -> extendParam more (ident:param:[])
        (Nothing, more)       -> (Nothing, more) -- Null case

-- Extends paramater list
extendParam (Semicolon:tx) params =
    case (parseIdentifier tx) of
        (Just nparam, more)   -> extendParam more (params ++ (nparam:[]))
        (Nothing, more)       -> (Nothing, more) -- Null case

-- Ends the paramater list
extendParam (RBrack:Equal:tx) params =
    case (parseSmLispExpr tx) of
        (Just expr, more) -> (Just (FunctionDef (head params) (tail params) expr), more) 
        (Nothing, more)   -> (Nothing, more) -- Null case
    

-- parseMain :: [Token] -> [SmLispProgram]