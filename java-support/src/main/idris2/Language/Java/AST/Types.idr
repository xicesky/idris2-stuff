
module Language.Java.AST.Types

%default total

------------------------------------------------------------------------------------------------------------------------

||| A single identifier.
public export
data Ident = JA_Ident String

||| A name, i.e. a period-separated list of identifiers.
public export
data Name = JA_Name (List Ident)

public export
data Diamond = JA_Diamond

------------------------------------------------------------------------------------------------------------------------

||| A primitive type is predefined by the Java programming language and named by its reserved keyword.
public export
data PrimType
    = JA_BooleanT
    | JA_ByteT
    | JA_ShortT
    | JA_IntT
    | JA_LongT
    | JA_CharT
    | JA_FloatT
    | JA_DoubleT

-- Forward declarations
data RefType : Type
data ClassType : Type
data TypeArgument : Type

------------------------------------------------------------------------------------------------------------------------

||| There are two kinds of types in the Java programming language: primitive types and reference types.
public export
data JavaType
    = JA_PrimType PrimType
    | JA_RefType RefType

||| There are three kinds of reference types: class types, interface types, and array types.
||| Reference types may be parameterized with type arguments.
||| Type variables cannot be syntactically distinguished from class type identifiers,
||| and are thus represented uniformly as single ident class types.
public export
data RefType
    = JA_ClassRefType ClassType
    {- | TypeVariable Ident -}
    | JA_ArrayType JavaType

||| A class or interface type consists of a type declaration specifier,
||| optionally followed by type arguments (in which case it is a parameterized type).
public export
data ClassType
    = JA_ClassType (List (Ident, List TypeArgument))

------------------------------------------------------------------------------------------------------------------------

public export
data TypeDeclSpecifier
    = JA_TypeDeclSpecifier ClassType
    | JA_TypeDeclSpecifierWithDiamond ClassType Ident Diamond
    | JA_TypeDeclSpecifierUnqualifiedWithDiamond Ident Diamond

||| Wildcards may be given explicit bounds, either upper (@extends@) or lower (@super@) bounds.
public export
data WildcardBound
    = JA_ExtendsBound RefType
    | JA_SuperBound RefType

||| Type arguments may be either reference types or wildcards.
public export
data TypeArgument
    = JA_Wildcard (Maybe WildcardBound)
    | JA_ActualType RefType

||| A class is generic if it declares one or more type variables. These type variables are known
||| as the type parameters of the class.
public export
data TypeParam = JA_TypeParam Ident (List RefType)

||| An exception type has to be a class type or a type variable.
export
ExceptionType : Type
ExceptionType = RefType -- restricted to ClassType or TypeVariable
