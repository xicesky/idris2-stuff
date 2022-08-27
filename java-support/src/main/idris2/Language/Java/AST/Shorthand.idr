
module Language.Java.AST.Shorthand

import Data.Maybe
import Language.Java.AST.Types
import Language.Java.AST.Expression

%default total

------------------------------------------------------------------------------------------------------------------------
-- Helpers
-- FIXME: Use List1 instead of FqClassName

export
split_list : List a -> Maybe (List a, a)
split_list list = sp list id where
    sp : List a -> (List a -> List a) -> Maybe (List a, a)
    sp []        _ = Nothing
    sp (x :: []) f = Just (f [], x)
    sp (x :: xs) f = sp xs (\rs => f (x :: rs))

public export
data FqClassName : Type -> Type where
    MkFqClassName : List a -> a -> FqClassName a

export
partial
className : List String -> FqClassName Ident
className fqn = let
    -- packageName : List String
    -- clsName : String
    Just (packageName, clsName) = split_list fqn
    in MkFqClassName (map JA_Ident packageName) (JA_Ident clsName)

------------------------------------------------------------------------------------------------------------------------
-- shorthand functions

export
java_name : List String -> Name
java_name = JA_Name . map JA_Ident

export
java_class : FqClassName Ident -> ClassBody -> CompilationUnit
java_class fqn classBody = let
    -- packageName : List Ident
    -- className : Ident
    MkFqClassName packageName className = fqn
    in JA_CompilationUnit
        (Just $ JA_PackageDecl $ JA_Name packageName)
        []  -- FIXME
        [JA_ClassTypeDecl $ JA_ClassDecl [] className [] Nothing [] classBody]
