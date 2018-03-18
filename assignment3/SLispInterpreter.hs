--Main program driver
--Uses the functions from the other files to interpret SLisp
--May be replaced as main driver by Evaluator.hs
import Tokenizer
import Parser
import Data.Maybe
import Environments
import Types

import System.Environment
import System.IO

sl_eval :: SmLispExpr -> Tree -> Tree -> SExpression
sl_eval (Variable ident)        fn_env value_env  = apply_env value_env ident
sl_eval (SExpr (NumAtom x))     fn_env value_env  = NumAtom x
sl_eval (SExpr (SymAtom str))   fn_env value_env  = SymAtom str
sl_eval (SExpr (List x))        fn_env value_env  = List x
sl_eval (FnCall ident args)     fn_env value_env  = SymAtom "T"--sl_apply ident (sl_evlis args fn_env value_env) fn_env (reset_to_global_frame value_env)
sl_eval (CondExpr clauses)      fn_env value_env  = sl_evcond clauses fn_env value_env
sl_eval (LetExpr defs expr)     fn_env value_env  = sl_eval expr fn_env (extend_local_env fn_env value_env defs)
sl_eval (MapExpr ident sexprl)  fn_env value_env  = lexprToSexpr(mapOp ident (sl_evlis (map SExpr sexprl) fn_env value_env) fn_env value_env)
sl_eval (ReduceExpr ident expr) fn_env value_env  = List[reduceOp ident (sl_evlis [expr] fn_env value_env) fn_env value_env]
sl_eval x fn_env value_env = error("Bad S-Lisp expression -->" ++(show x))

extend_env :: Tree -> Identifier -> Definition -> Tree
extend_env env ident def = treeInsert env (ident,def)

apply_env :: Tree -> Identifier -> SExpression
apply_env env name =
  case treeLookup env name of
    Nothing                           -> error("Undefined symbol -->" ++ name)
    Just(ConstantDef ident (SExpr x)) -> x

--reset_to_global_frame:: Tree -> Tree
--reset_to_global_frame env = 

sl_evlis:: [SmLispExpr] -> Tree -> Tree -> [SmLispExpr]
sl_evlis expr fn_env value_env = map SExpr (map ($value_env) (map ($fn_env) (map (sl_eval) expr)))

--TO DO: Implement sl_evcond using a suitable predicate iteration function from the Haskell library. 
sl_evcond:: [CondClause] -> Tree -> Tree -> SExpression
sl_evcond ((Clause pred res):rest) fn_env value_env
  | sl_eval pred fn_env value_env == SymAtom "T" = sl_eval res fn_env value_env
  | rest == [] = error("Error in conditional expression -- no true predicate")
  | otherwise  = sl_evcond rest fn_env value_env

extend_local_env:: Tree -> Tree -> [LocalDef] -> Tree
extend_local_env fn_env value_env ((Binding ident expr):rest)
  | rest == [] = extend_env value_env ident (ConstantDef ident (SExpr(sl_eval expr fn_env value_env)))
  | otherwise  = extend_env (extend_local_env fn_env value_env rest) ident (ConstantDef ident (SExpr(sl_eval expr fn_env value_env)))
  
lexprToSexpr:: [SmLispExpr] -> SExpression
extender:: [SmLispExpr] -> [SExpression]

lexprToSexpr [SExpr a] = a
lexprToSexpr ((SExpr a): rest)
  | rest == [] = List [a]
  | otherwise  = List ([a] ++ (extender rest))

extender ((SExpr a): rest)
  | rest == [] = [a]
  | otherwise  = [a] ++ (extender rest)
  
mapOp:: Identifier -> [SmLispExpr] -> Tree -> Tree -> [SmLispExpr]
mapOp func lists fn_env value_env 
  | elem (Just (SExpr (List[]))) (map apply_rest (map (:[])lists)) = [fromJust (sl_apply func (map fromJust (map apply_first (map (:[]) lists))) fn_env value_env)]
  | otherwise = [fromJust (sl_apply func (map fromJust (map apply_first (map (:[]) lists))) fn_env value_env)] ++ mapOp func (map fromJust (map apply_rest (map (:[])lists))) fn_env value_env
  
reduceOp:: Identifier -> [SmLispExpr] -> Tree -> Tree -> SExpression
reduceOp func [SExpr (List [])] fn_env value_env = error("Not a valid list for reducing")
reduceOp func [SExpr (List (a:b:rest))] fn_env value_env
  | rest == [] = lexprToSexpr([fromJust(sl_apply func [SExpr a, SExpr b] fn_env value_env)])
  | otherwise  = reduceOp func [SExpr(List([   lexprToSexpr[fromJust(sl_apply func [SExpr a, SExpr b] fn_env value_env)]   ] ++ rest))] fn_env value_env
--------------------------------------------------------------------------------------------

sl_apply :: Identifier -> [SmLispExpr] -> Tree -> Tree -> Maybe (SmLispExpr)
sl_apply callee (arg:rest) fn_env value_env
  | callee == "first" = apply_first[arg]
  | callee == "rest" = apply_rest[arg]
  | callee == "endp" = apply_endp[arg]
  | callee == "numberp" = apply_numberp[arg]
  | callee == "symbolp" = apply_symbolp[arg]
  | callee == "listp" = apply_listp[arg]
  | callee == "eq" = apply_eq[arg, head rest]
  | callee == "cons" = apply_cons[arg, head rest]
  | callee == "plus" = apply_plus[arg, head rest]
  | callee == "minus" = apply_minus[arg, head rest]
  | callee == "times" = apply_times[arg, head rest]
  | callee == "divide" = apply_divide[arg, head rest]
  | callee == "rem" = apply_rem[arg, head rest]
  | callee == "eqp" = apply_eqp[arg, head rest]
  | callee == "lessp" = apply_lessp[arg, head rest]
  | callee == "greaterp" = apply_greaterp[arg, head rest]
  | callee == "sym-lessp" = apply_sym_lessp[arg, head rest]
  | callee == "explode" = apply_explode[arg]
  | callee == "implode" = apply_implode[arg]
  | callee == "error" = apply_error[arg]
  | otherwise = Just (SExpr(sl_eval (fst(apply_fn_env fn_env callee)) fn_env (add_associations value_env (snd(apply_fn_env fn_env callee)) (arg:rest))))
  
apply_fn_env :: Tree -> Identifier -> (SmLispExpr, [Identifier])
apply_fn_env env name =
  case treeLookup env name of
    Nothing                             -> error("Undefined function -->" ++ name)
    Just(FunctionDef ident params body) -> (body, params)

add_associations :: Tree -> [Identifier] -> [SmLispExpr] -> Tree
add_associations env (param:restp) (arg:resta)
  | restp == [] = extend_env env param (ConstantDef param arg)
  | otherwise   = add_associations (extend_env env param (ConstantDef param arg)) restp resta

--------------------------------------------------------------------------------------------
main = do
    fileName <- getArgs
    srcText <- readFile (head fileName)
    -- Output from Tokenizer
    --print (tokenizeMain(['\n'] ++srcText))
    -- Output from Parser
    print (parseSExpression(fromMaybe [] (tokenizeMain(['\n'] ++srcText))))

interactive :: [Char] -> (Maybe SmLispExpr, [Token])
interactive tx = parseExpression (tokenizeMain tx)

definitionGen :: [Char] -> (Maybe SmLispProgram, [Token])
definitionGen tx = parseSmLispProgram (tokenizeMain tx)

interpret :: Definition -> Expression -> Maybe SExpression
interpret defs expr = setup_envs_then_eva(Empty, extend_env(extend_env(extend_env(Empty "F" "F") "T" "T") "Otherwise" "T") defs expr )

extend_env :: Tree -> Identifier -> Definition -> Tree
extend_env end ident def = treeInsert env (ident,def)

setup_envs_then_eval :: Tree -> Tree -> [Char] -> [Char] -> Maybe SExpression
setup_envs_then_eval fn_env val_env defs expr =
  | null(defs) = sl_eval(expr fn_env mark_global_frame(val_env))
  |
  |

key :: Tree -> Node
key tree = tree !! 0

left_env :: Tree -> Node
left_env tree = tree !! 1

right_env :: Tree -> Node
right_env tree = tree !! 2

key_value :: Tree -> Node
key_value tree = tree !! 3

local_frame :: Tree -> Node
local_frame env = env !! 0

global_frame :: Tree -> Node
global_frame env = env !! 1

mark_global_frame :: Tree -> Tree
mark_global_frame env = make_value_env(Empty local_frame(env))

reset_to_global_frame :: Tree -> Tree
reset_to_global_frame env = make_value_env(Empty global_frame(env))

make_value_env :: Tree ->Tree -> Tree
make_value_env l_frame g_frame = l_frame++g_frame
