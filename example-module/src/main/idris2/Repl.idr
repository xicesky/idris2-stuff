module Repl

-- Only for use at the repl
-- e.g.: pack --with-ipkg example-module.ipkg repl src/main/idris2/Repl.idr

import public Data.Maybe
import public Data.List1

import public Main

replDemo : IO ()
replDemo = putStrLn "Hello world in repl"
