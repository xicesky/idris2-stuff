
module Language.Java.AST.Shorthand

import Data.Maybe
import Language.Java.AST.Types
import Language.Java.AST.Expression

%default total

------------------------------------------------------------------------------------------------------------------------
-- Helpers
-- FIXME: Use List1 instead of FqClassName

export
implementation FromString Ident where
    fromString = JA_Ident

public export
interface ToIdent a where
    toIdent : a -> Ident

export
implementation ToIdent Ident where
    toIdent = id

export
implementation ToIdent String where
    toIdent = JA_Ident

export
split_list : List a -> Maybe (List a, a)
split_list list = sp list id where
    sp : List a -> (List a -> List a) -> Maybe (List a, a)
    sp []        _ = Nothing
    sp (x :: []) f = Just (f [], x)
    sp (x :: xs) f = sp xs (\rs => f (x :: rs))

-- List1 is just clumsy here because we mostly want `last` and `init`
public export
data FqClassName : Type -> Type where
    MkFqClassName : List a -> a -> FqClassName a

export
partial
className : (ToIdent pkgIdentT, ToIdent classIdentT) => List pkgIdentT -> classIdentT -> FqClassName Ident
className packageName clsName = MkFqClassName (map toIdent packageName) (toIdent clsName)

------------------------------------------------------------------------------------------------------------------------
-- the usual suspect types

export
java_boolean : JavaType
java_boolean = JA_PrimType JA_BooleanT

export
java_byte : JavaType
java_byte = JA_PrimType JA_ByteT

export
java_short : JavaType
java_short = JA_PrimType JA_ShortT

export
java_int : JavaType
java_int = JA_PrimType JA_IntT

export
java_long : JavaType
java_long = JA_PrimType JA_LongT

export
java_char : JavaType
java_char = JA_PrimType JA_CharT

export
java_float : JavaType
java_float = JA_PrimType JA_FloatT

export
java_double : JavaType
java_double = JA_PrimType JA_DoubleT

------------------------------------------------------------------------------------------------------------------------
-- shorthand functions

export
java_name : ToIdent identT => List identT -> Name
java_name = JA_Name . map toIdent

export
private_field : ToIdent identT => JavaType -> identT -> Maybe VarInit -> Decl
private_field jType ident init = JA_MemberDecl $ JA_FieldDecl [JA_Private] jType [JA_VarDecl (JA_VarId $ toIdent ident) init]

export
public_constructor : ToIdent identT => identT -> List BlockStmt -> Decl
public_constructor ident body = let
    formalParams : List FormalParam
    formalParams = []
    exceptions : List ExceptionType
    exceptions = []
    in JA_MemberDecl $ JA_ConstructorDecl [JA_Public] [] (toIdent ident) formalParams exceptions $ JA_ConstructorBody Nothing body

export
generated_modifier : Modifier
generated_modifier = JA_Annotation $ JA_MarkerAnnotation $ MkMarkerAnnotation $ java_name ["net", "q1cc", "sky", "Generated"]

export
immutable_modifier : Modifier
immutable_modifier = JA_Annotation $ JA_MarkerAnnotation $ MkMarkerAnnotation $ java_name ["net", "q1cc", "sky", "Immutable"]

export
java_class : FqClassName Ident -> ClassBody -> CompilationUnit
java_class fqn classBody = let
    -- packageName : List Ident
    -- className : Ident
    MkFqClassName packageName className = fqn
    in JA_CompilationUnit
        (Just $ JA_PackageDecl $ JA_Name packageName)
        []  -- FIXME imports
        [JA_ClassTypeDecl $ JA_ClassDecl [generated_modifier, JA_Public, immutable_modifier] className [] Nothing [] classBody]
