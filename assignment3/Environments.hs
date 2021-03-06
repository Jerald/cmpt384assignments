--Deals with environment management

--Requirements:
--Contains functions for dealing with function and variable environments
--Implements some as-of-yet undecided data structure to store said environments
module Environments where


import Data.List
import Data.Maybe
import Data.Char
import Types

import Control.Exception

--To represent environments for our small lisp interpreter, we will be using a binary search tree, with each
--node represented as a key-value pair

type Node = (Identifier, Definition)
data Tree = Branch Tree Node Tree | Empty deriving (Show, Eq)

treeInsert :: Tree -> Node -> Tree
treeInsert Empty newNode@(ident, def) = Branch Empty newNode Empty
treeInsert (Branch left node@(ident, def) right) newNode@(newIdent, newDef)
    | newIdent == ident = Branch left newNode right
    | newIdent <  ident = Branch (treeInsert left newNode) node right
    | newIdent >  ident = Branch left node (treeInsert right newNode)
    | otherwise         = error("God is dead, strings don't compare to strings")

treeLookup :: Tree -> Identifier -> Maybe Definition
treeLookup Empty ident = Nothing
treeLookup (Branch left node@(ident, def) right) fIdent
    | fIdent == ident = Just def
    | fIdent  < ident = treeLookup left fIdent
    | fIdent  > ident = treeLookup right fIdent
    | otherwise       = error("God is dead, strings don't compare to strings")

treeToList :: Tree -> [Node]
treeToList Empty = []
treeToList (Branch left node right) = (treeToList left) ++ (node:[]) ++ (treeToList right)
  
--Implementing the 20 primitive functions of small lisp

-- Returns T if the input is a SymAtom, F if a NumAtom or List. Nothing otherwise.
apply_symbolp :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_symbolp ((SExpr (SymAtom a)):[])  = Just (SExpr (SymAtom "T"))
apply_symbolp ((SExpr a):[])            = Just (SExpr (SymAtom "F"))
apply_symbolp _                         = Nothing

-- Returns T if the input is a NumAtom, F if a SymAtom or List. Nothing otherwise.
apply_numberp :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_numberp [SExpr (NumAtom a)]       = Just (SExpr (SymAtom "T"))
apply_numberp [SExpr a]                 = Just (SExpr (SymAtom "F"))
apply_numberp _                         = Nothing  

-- Returns T if the input is a List, F if a NumAtom or SymAtom. Nothing otherwise.
apply_listp :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_listp [SExpr (List a)]            = Just (SExpr (SymAtom "T"))
apply_listp [SExpr a]                  = Just (SExpr (SymAtom "F"))
apply_listp _                           = Nothing

-- Returns T if the input is empty, F if not.  Nothing otherwise.
apply_endp :: [SmLispExpr] -> Maybe(SmLispExpr)
apply_endp [SExpr (List a)]
  | null(a)                             = Just (SExpr (SymAtom "T"))
  | otherwise                           = Just (SExpr (SymAtom "F"))
apply_endp _ = Nothing


apply_first :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_first [SExpr (List (a:rest))]     = Just (SExpr a)
apply_first _                           = Nothing

apply_rest :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_rest [SExpr (List (a:rest))]      = Just (SExpr (List rest))
apply_rest _ = Nothing

apply_cons :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_cons [SExpr x, SExpr (List y)]    = Just (SExpr (List (x:y)))
apply_cons _ = Nothing

apply_eq :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_eq [SExpr (SymAtom x), SExpr (SymAtom y)]
  | x == y                              = Just (SExpr (SymAtom "T"))
  | otherwise                           = Just (SExpr (SymAtom "F"))
apply_eq _ = Nothing

apply_plus :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_plus [SExpr (NumAtom x), SExpr (NumAtom y)]   = Just (SExpr (NumAtom(x+y)))
apply_plus _                                        = Nothing

apply_minus :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_minus [SExpr (NumAtom x), SExpr (NumAtom y)]  = Just (SExpr (NumAtom(x-y)))
apply_minus _                                       = Nothing

apply_times :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_times [SExpr (NumAtom x), SExpr (NumAtom y)]  = Just (SExpr (NumAtom(x*y)))
apply_times _                                       = Nothing

apply_divide :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_divide [SExpr (NumAtom x), SExpr (NumAtom y)]
  | y /= 0                  = Just (SExpr (NumAtom(quot x y)))
  | otherwise               = Nothing
apply_divide _              = Nothing

apply_rem :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_rem [SExpr (NumAtom x), SExpr (NumAtom y)]
  | y /= 0                  = Just (SExpr (NumAtom(rem x y)))
  | otherwise               = Nothing
apply_rem _                 = Nothing

apply_eqp :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_eqp [SExpr (NumAtom x), SExpr (NumAtom y)]
  | x == y                  = Just (SExpr (SymAtom "T"))
  | otherwise               = Just (SExpr (SymAtom "F"))
apply_eqp _                 = Nothing

apply_lessp :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_lessp [SExpr (NumAtom x), SExpr (NumAtom y)]
  | x < y                   = Just (SExpr (SymAtom "T"))
  | otherwise               = Just (SExpr (SymAtom "F"))
apply_lessp _               = Nothing

apply_greaterp :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_greaterp [SExpr (NumAtom x), SExpr (NumAtom y)]
  | x < y                   = Just (SExpr (SymAtom "T"))
  | otherwise               = Just (SExpr (SymAtom "F"))
apply_greaterp _            = Nothing

apply_sym_lessp :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_sym_lessp [SExpr (SymAtom x), SExpr (SymAtom y)]
  | x < y                   = Just (SExpr (SymAtom "T"))
  | otherwise               = Just (SExpr (SymAtom "F"))
apply_sym_lessp _           = Nothing

------------------------------------------------------------------------

apply_explode :: [SmLispExpr] -> Maybe (SmLispExpr)
explode_extend :: [SExpression] -> [SExpression]

apply_explode [SExpr (SymAtom (x:rest))]
  | rest == [] && isDigit(x)    = Just (SExpr (List[NumAtom (read[x])]))
  | rest == []                  = Just (SExpr (List[SymAtom [x]]))
  | isDigit(x)                  = Just (SExpr (List(NumAtom (read [x]) : explode_extend([SymAtom rest]))))
  | otherwise                   = Just (SExpr (List(SymAtom [x] : explode_extend([SymAtom rest]))))
apply_explode [SExpr (SymAtom x)]
  | x == []                     = Just (SExpr (List[]))
  | otherwise                   = Nothing
apply_explode _                 = Nothing

explode_extend [SymAtom (x:rest)]
  | rest == [] && isDigit(x)    = [NumAtom (read[x])]
  | rest == []                  = [SymAtom [x]]
  | isDigit(x)                  = (NumAtom (read[x]) : explode_extend([SymAtom rest]))
  | otherwise                   = ((SymAtom [x] : explode_extend([SymAtom rest])))
  
---------------------------------------------------------------------------
atomToStr:: SExpression -> [Char]
atomToStr (SymAtom x) = x
atomToStr (NumAtom x) = show x

apply_implode :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_implode [SExpr (List (x))]
  | apply_numberp[SExpr (head x)] == Just(SExpr (SymAtom "T"))                      = Nothing
  | elem (Just (SExpr (SymAtom "T"))) (map apply_listp (map (:[]) (map SExpr x)))   = Nothing
  | otherwise                       = Just (SExpr (SymAtom (intercalate "" (map atomToStr x))))
apply_implode _                     = Nothing

-----------------------------------------------------------------------------

apply_error :: [SmLispExpr] -> Maybe (SmLispExpr)
apply_error [SExpr (SymAtom x)] = error x
apply_error _                   = Nothing
