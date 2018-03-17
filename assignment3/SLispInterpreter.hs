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

--------------------------------------------------------------------------------------------
fn_env = Tip
val_env = Tip

global_val_env = Tree

--Function definition is wrong, should be FnCall Tree a -> Tree b -> Maybe (SExpression)
--Unsure how to convert a 'variable' into a 'SExpression' type
sl_apply :: SmLispExpr -> Tree a -> Tree b -> Maybe (SmLispExpr)
sl_apply (FnCall callee args) fn_env val_env
  | callee == "first" = apply_first[args!!0]
  | callee == "rest" = apply_rest[args!!0]
  | callee == "endp" = apply_endp[args!!0]
  | callee == "numberp" = apply_numberp[args!!0]
  | callee == "symbolp" = apply_symbolp[args!!0]
  | callee == "listp" = apply_listp[args!!0]
  | callee == "eq" = apply_eq[args!!0, args!!1]
  | callee == "cons" = apply_cons[args!!0, args!!1]
  | callee == "plus" = apply_plus[args!!0, args!!1]
  | callee == "minus" = apply_minus[args!!0, args!!1]
  | callee == "times" = apply_times[args!!0, args!!1]
  | callee == "divide" = apply_divide[args!!0, args!!1]
  | callee == "rem" = apply_rem[args!!0, args!!1]
  | callee == "eqp" = apply_eqp[args!!0, args!!1]
  | callee == "lessp" = apply_lessp[args!!0, args!!1]
  | callee == "greaterp" = apply_greaterp[args!!0, args!!1]
  | callee == "sym-lessp" = apply_sym_lessp[args!!0, args!!1]
  | callee == "explode" = apply_explode[args!!0]
  | callee == "implode" = apply_implode[args!!0]
  | callee == "error" = apply_error[args!!0]
  | otherwise = Nothing
  -- Otherwise, get the definition of the function from fn_env, then use
  --sl-eval[expr; fn-env; value-env] on it (Adding all parameters to value-env
--------------------------------------------------------------------------------------------


main = do
    [defs, expr] <- getArgs
    defsString <- readFile (defs)
    exprString <- readFile (expr)
    -- Output from Tokenizer
    print (tokenizeMain(['\n'] ++srcText))
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
