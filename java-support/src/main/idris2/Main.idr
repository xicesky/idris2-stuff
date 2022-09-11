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
    (JA_ClassBody
        [ JA_MemberDecl (JA_FieldDecl [JA_Public] (JA_PrimType JA_IntT) [JA_VarDecl (JA_VarId (JA_Ident "x")) Nothing])
        ]
    )

------------------------------------------------------------------------------------------------------------------------

main : IO ()
main = do
    prettyPrint exampleClass
    -- putdoc exampleDoc
