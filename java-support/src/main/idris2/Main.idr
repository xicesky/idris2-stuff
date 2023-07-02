module Main

import Language.Java.AST.Types
import Language.Java.AST.Expression
import Language.Java.AST.Shorthand
import Language.Java.Pretty

------------------------------------------------------------------------------------------------------------------------

%default partial

export
examplePackage : List String
examplePackage = ["net","q1cc","sky"]

export
exampleClass : CompilationUnit
exampleClass = java_class
    (className examplePackage "Example")
    (JA_ClassBody
        [ private_field java_int (JA_Ident "x") Nothing
        , private_field java_double (JA_Ident "y") Nothing
        , public_constructor (JA_Ident "dummy")
            [ JA_BlockStmt $ JA_Empty
            , JA_BlockStmt $ JA_ExpStmt $ JA_Assign (JA_NameLhs $ java_name ["x"]) JA_EqualA (JA_Lit $ JA_Int 3)
            ]
        ]
    )

------------------------------------------------------------------------------------------------------------------------

export
main : IO ()
main = do
    prettyPrint exampleClass
    -- putdoc exampleDoc
