--Parses tokenized input stream

--Requirements:
--Uses a token stream to produce haskell representation of SLisp
--Should use SLisp data types defined elsewhere and parse into said types
--Most likely needs one function per said type

-- parseSmLispProgram builds out a definition list. Use it for building environments.
-- parseSmLispExpr parses an SLisp expression. Primarily evaluation function.

module Parser where

import System.IO
import System.Environment
import Data.Maybe
import Data.Char
import Data.Typeable

import Types
    
parseSExpression    :: [Token] -> (Maybe SExpression, [Token])
extendList          :: [Token] -> SExpression -> (Maybe SExpression, [Token])

parseSExpression ((NumToken t):tx) = (Just (NumAtom t), tx)

parseSExpression ((AlphaNumToken t):tx) = (Just (SymAtom t), tx)
parseSExpression ((SpecialToken t):tx) = (Just (SymAtom t), tx)

parseSExpression (LParen:t:tx) = parseList (LParen:t:tx)

parseSExpression tx = (Nothing, tx)


parseList :: [Token] -> (Maybe SExpression, [Token])
parseList (LParen:t:tx)
    | t == RParen           = (Just (List []), tx) -- We may just be able to get rid of this
    | otherwise             = extendList (t:tx) (List [])
parseList tx = (Nothing, tx)

-- End case, might need to change

extendList (RParen:tx) list = (Just list, tx)
extendList tx (List list) = 
    case parseSExpression tx of
        (Just sexpr, more)  -> extendList more (List (list ++ sexpr:[]))
        (Nothing, more)     -> (Nothing, more)
extendList tx _ = (Nothing, tx)

--------------------------------------------------------------------

parseIdentifier     :: [Token] -> (Maybe Identifier, [Token])
-- parseLocalDef       :: [Token] -> (Maybe LocalDef, [Token])
parseSmLispExpr     :: [Token] -> (Maybe SmLispExpr, [Token])

parseIdentifier []                      = (Nothing, [])
parseIdentifier ((AlphaNumToken t):tx)  = (Just t, tx)
parseIdentifier tx                      = (Nothing, tx)


-- parseLocalDef []                        = (Nothing, [])
-- parseLocalDef -- Deal with this as a part of SmLispExpr
-- parseLocalDef tx                        = (Nothing, tx)

parseSmLispExpr [] = (Nothing, [])

parseSmLispExpr ((Comment t):tx) =
    parseSmLispExpr tx

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
    case buildLocalDefs (Semicolon:tx) [] of
        (Just defs, more) -> endLetExpr more defs
        (Nothing, more) -> (Nothing, more)

parseSmLispExpr (t:(AlphaNumToken ident):LBrack:tx)
    | t == At       = parseMap tx ident
    | t == Exclam   = parseReduce tx ident

-- End case
parseSmLispExpr tx =
    typeWrap SExpr (parseSExpression tx)

parseReduce :: [Token] -> Identifier -> (Maybe SmLispExpr, [Token])
parseReduce tx ident = 
    case parseSmLispExpr tx of
        (Just expr, RBrack:more)    -> (Just (ReduceExpr ident expr), more)
        (_, more)                   -> (Nothing, more)

parseMap    :: [Token] -> Identifier -> (Maybe SmLispExpr, [Token])
parseMap tx ident =
    case extendMapArgs (Semicolon:tx) [] of
        (Just args, more) -> (Just (MapExpr ident args), more)
                            -- if (verifyArgLen args)
                            --     then (Just (MapExpr ident args), more)
                            --     else (Nothing, [AlphaNumToken "Death"])
        (Nothing, more) -> (Nothing, more)
        
extendMapArgs :: [Token] -> [SExpression] -> (Maybe [SExpression], [Token])
extendMapArgs (RBrack:tx) args = (Just args, tx)
extendMapArgs (Semicolon:tx) args =
    case parseList tx of
        (Just sexpr, more) -> extendMapArgs more (args ++ (sexpr:[]))
        (Nothing, more) -> (Nothing, more)
extendMapArgs tx args = (Nothing, tx)

verifyArgLen :: [SExpression] -> Bool
verifyArgLen (args) =
    let checked = [(List x) | (List x) <- (tail args), length x == length first]
    in length checked == length args
    where (List first) = head args
-- verifyArgLen args = True

endLetExpr :: [Token] -> [LocalDef] -> (Maybe SmLispExpr, [Token])
endLetExpr tx defs =
    case parseSmLispExpr tx of
        (Just expr, RBrace:more) -> (Just (LetExpr defs expr), more)
        (_, more) -> (Nothing, more)    
        
parseLocalDef :: [Token] -> (Maybe LocalDef, [Token])
parseLocalDef ((AlphaNumToken alph):Equal:tx) = 
    case parseSmLispExpr tx of
        (Just expr, more) -> (Just (Binding ident expr), more)
        (Nothing, more) -> (Nothing, more)
    where (Just ident, _) = parseIdentifier ((AlphaNumToken alph):[])

parseLocalDef tx = (Nothing, tx)

buildLocalDefs :: [Token] -> [LocalDef] -> (Maybe [LocalDef], [Token])
buildLocalDefs (Colon:tx) defs = (Just defs, tx)
buildLocalDefs (Semicolon:tx) defs = 
    case parseLocalDef tx of
        (Just ndef, (more)) -> buildLocalDefs more (defs ++ (ndef:[]))
        (Nothing, more) -> (Nothing, more)
buildLocalDefs tx defs = (Nothing, tx)

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
extendCondExpr (Semicolon:tx) (CondExpr clauses) =
    case parseCondClause tx of
        (Just clause, more)    -> extendCondExpr more (CondExpr (clauses ++ (clause:[])))
        (Nothing, more)        -> (Nothing, more) 
extendCondExpr (RBrack:tx) (CondExpr clauses) = (Just (CondExpr clauses), tx)
extendCondExpr tx expr = (Nothing, tx)


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

-- Entry point for Parser.hs
parseSmLispProgram (Just tx) = buildDefinition tx []
parseSmLispProgram (Nothing) = (Nothing, [])

buildDefinition [] program = (Just program, [])
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

parseDefinition ((Comment comm):tx) = 
    parseDefinition tx

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
    
extendParam tx params = (Nothing, tx)

parseExpression :: Maybe [Token] -> (Maybe SmLispExpr, [Token])
parseExpression (Just tx) = parseSmLispExpr tx
parseExpression (Nothing) = (Nothing, [])
-- parseMain :: [Token] -> [SmLispProgram]