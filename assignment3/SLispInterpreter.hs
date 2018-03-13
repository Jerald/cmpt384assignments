--Main program driver
--Uses the functions from the other files to interpret SLisp
--May be replaced as main driver by Evaluator.hs
import Tokenizer
import Parser
import Data.Maybe

import System.Environment
import System.IO
main = do
    fileName <- getArgs
    srcText <- readFile (head fileName)
    -- Output from Tokenizer
    print (tokenizeMain(['\n'] ++srcText))
    -- Output from Parser
    print (parseSExpression(fromMaybe [] (tokenizeMain(['\n'] ++srcText))))
