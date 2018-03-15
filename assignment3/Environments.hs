--Deals with environment management

--Requirements:
--Contains functions for dealing with function and variable environments
--Implements some as-of-yet undecided data structure to store said environments

import Data.List
import Data.Maybe
import Data.Char
import Types

--To represent environments for our small lisp interpreter, we will be using a binary search tree, with each
--node represented as a key-value pair

data BSTree a = Tip | Node (BSTree a) (a, a) (BSTree a) deriving (Show, Eq)

treeInsert:: (Ord a) => BSTree a -> (a, a) -> BSTree a
treeInsert Tip (new_key, new_value) = Node Tip (new_key, new_value) Tip
treeInsert (Node l (cur_key, cur_value) r) (new_key, new_value)
  | new_key <= cur_key = Node (treeInsert l (new_key, new_value)) (cur_key, cur_value) r
  | otherwise = Node l (cur_key, cur_value) (treeInsert r (new_key, new_value))

treeLookup:: (Ord a) => BSTree a -> a -> Maybe (a)
treeLookup Tip key = Nothing
treeLookup (Node l (cur_key, cur_value) r) key
  | key == cur_key = Just cur_value
  | key <= cur_key = treeLookup l key
  | otherwise = treeLookup r key
  
treeToList:: BSTree a -> [(a, a)]
treeToList tree = case tree of
  Tip -> []
  Node l v r -> treeToList(l) ++ [v] ++ treeToList(r)
  
--Implementing the 20 primitive functions of small lisp

instance Eq SExpression where
  (SymAtom x) == (SymAtom y) = x == y

apply_symbolp :: [SExpression] -> Maybe (SExpression)
apply_symbolp [(SymAtom a)] = Just (SymAtom "T")
apply_symbolp [a] = Just (SymAtom "F")

apply_numberp :: [SExpression] -> Maybe (SExpression)
apply_numberp [(NumAtom a)] = Just (SymAtom "T")
apply_numberp [a] = Just (SymAtom "F")
  
apply_listp :: [SExpression] -> Maybe (SExpression)
apply_listp [(List a)] = Just (SymAtom "T")
apply_listp [a] = Just (SymAtom "F")

apply_endp :: [SExpression] -> Maybe(SExpression)
apply_endp [(List a)]
  | null(a) = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
apply_endp [a] = Nothing

apply_first :: [SExpression] -> Maybe (SExpression)
apply_first [List (a:rest)] = Just a
apply_first [a] = Nothing

apply_rest :: [SExpression] -> Maybe (SExpression)
apply_rest [List (a:rest)] = Just (List rest)
apply_rest [a] = Nothing

apply_cons :: [SExpression] -> Maybe (SExpression)
apply_cons [x, List y] = Just (List (x:y))
apply_cons [x, y] = Nothing

apply_eq :: [SExpression] -> Maybe (SExpression)
apply_eq [SymAtom x, SymAtom y]
  | x==y = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
apply_eq [x, y] = Nothing

apply_plus :: [SExpression] -> Maybe (SExpression)
apply_plus [NumAtom x, NumAtom y] = Just (NumAtom(x+y))
apply_plus [x, y] = Nothing

apply_minus :: [SExpression] -> Maybe (SExpression)
apply_minus [NumAtom x, NumAtom y] = Just (NumAtom(x-y))
apply_minus [x, y] = Nothing

apply_times :: [SExpression] -> Maybe (SExpression)
apply_times [NumAtom x, NumAtom y] = Just (NumAtom(x*y))
apply_times [x, y] = Nothing

apply_divide :: [SExpression] -> Maybe (SExpression)
apply_divide [NumAtom x, NumAtom y]
  | y /= 0 = Just (NumAtom(quot x y))
  | otherwise = Nothing
apply_divide [x, y] = Nothing

apply_rem :: [SExpression] -> Maybe (SExpression)
apply_rem [NumAtom x, NumAtom y]
  | y /= 0 = Just (NumAtom(rem x y))
  | otherwise = Nothing
apply_rem [x, y] = Nothing

apply_eqp :: [SExpression] -> Maybe (SExpression)
apply_eqp [NumAtom x, NumAtom y]
  | x==y = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
apply_eqp [x, y] = Nothing

apply_lessp :: [SExpression] -> Maybe (SExpression)
apply_lessp [NumAtom x, NumAtom y]
  | x<y = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
apply_lessp [x, y] = Nothing

apply_greaterp :: [SExpression] -> Maybe (SExpression)
apply_greaterp [NumAtom x, NumAtom y]
  | x<y = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
apply_greaterp [x, y] = Nothing

apply_sym_lessp :: [SExpression] -> Maybe (SExpression)
apply_sym_lessp [SymAtom x, SymAtom y]
  | x<y = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
apply_sym_lessp [x, y] = Nothing

------------------------------------------------------------------------

apply_explode :: [SExpression] -> Maybe (SExpression)
explode_extend :: [SExpression] -> [SExpression]

apply_explode [SymAtom (x:rest)]
  | rest == [] && isDigit(x) = Just (List[NumAtom (read[x])])
  | rest == [] = Just (List[SymAtom [x]])
  | isDigit(x) = Just (List(NumAtom (read [x]) : explode_extend([SymAtom rest])))
  | otherwise = Just (List(SymAtom [x] : explode_extend([SymAtom rest])))
apply_explode [SymAtom x]
  | x == [] = Just (List[])
  | otherwise = Nothing
apply_explode [x] = Nothing

explode_extend [SymAtom (x:rest)]
  | rest == [] && isDigit(x) = [NumAtom (read[x])]
  | rest == [] = [SymAtom [x]]
  | isDigit(x) = (NumAtom (read[x]) : explode_extend([SymAtom rest]))
  | otherwise = (SymAtom [x] : explode_extend([SymAtom rest]))
  
---------------------------------------------------------------------------
atomToStr:: SExpression -> [Char]
atomToStr (SymAtom x) = x
atomToStr (NumAtom x) = show x

apply_implode :: [SExpression] -> Maybe (SExpression)
apply_implode [List (x)]
  | (apply_numberp[(head x)]) == Just(SymAtom "T") = Nothing
  | elem (Just (SymAtom "T")) (map apply_listp (map (:[]) x))  = Nothing
  | otherwise = Just (SymAtom (intercalate "" (map atomToStr x)))
apply_implode [x] = Nothing

-----------------------------------------------------------------------------

apply_error :: [SExpression] -> Maybe (SExpression)
apply_error [SymAtom x] = error x
