module Main

import Language.Java.AST.Types
import Language.Java.AST.Expression
import Language.Java.AST.Shorthand
import Language.Java.Pretty

------------------------------------------------------------------------------------------------------------------------

%default partial

exampleClass : CompilationUnit
exampleClass = java_class
    (className ["net","q1cc","sky","Example"])
    (JA_ClassBody [])

------------------------------------------------------------------------------------------------------------------------

main : IO ()
main = do
    prettyPrint exampleClass
    -- putDoc exampleDoc
