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

--Function definition is wrong, should be FnCall BSTree a -> BSTree b -> Maybe (SExpression)
--Unsure how to convert a 'variable' into a 'SExpression' type
sl_apply :: [Char] -> SExpression -> BSTree a -> BSTree b -> Maybe (SExpression)
sl_apply callee (List args) fn_env val_env
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
    fileName <- getArgs
    srcText <- readFile (head fileName)
    -- Output from Tokenizer
    print (tokenizeMain(['\n'] ++srcText))
    -- Output from Parser
    print (parseSExpression(fromMaybe [] (tokenizeMain(['\n'] ++srcText))))
