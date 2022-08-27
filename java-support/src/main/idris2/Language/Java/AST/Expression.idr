module Language.Java.AST.Expression

-- import Generics.Derive -- "Language.Reflection.TT.Name" conflicts with "Language.Java.AST.Types.Name"

import Language.Java.AST.Types
-- import Language.Java.AST.Types as J

%default total

-- FIXME: License?
-- Partially from https://github.com/vincenthz/language-java

------------------------------------------------------------------------------------------------------------------------

||| A literal denotes a fixed, unchanging value.
public export
data Literal
    = JA_Int Integer
    | JA_Word Integer
    | JA_Float Double
    | JA_Double Double
    | JA_Boolean Bool
    | JA_Char Char
    | JA_String String
    | JA_Null

||| A binary infix operator.
public export
data Op = JA_Mult | JA_Div | JA_Rem | JA_Add | JA_Sub | JA_LShift | JA_RShift | JA_RRShift
        | JA_LThan | JA_GThan | JA_LThanE | JA_GThanE | JA_Equal | JA_NotEq
        | JA_And | JA_Or | JA_Xor | JA_CAnd | JA_COr

||| An assignment operator.
public export
data AssignOp = JA_EqualA | JA_MultA | JA_DivA | JA_RemA | JA_AddA | JA_SubA
              | JA_LShiftA | JA_RShiftA | JA_RRShiftA | JA_AndA | JA_XorA | JA_OrA

------------------------------------------------------------------------------------------------------------------------

-- Foward declaration
public export
data Annotation : Type

||| A modifier specifying properties of a given declaration. In general only
||| a few of these modifiers are allowed for each declaration type, for instance
||| a member type declaration may only specify one of public, private or protected.
public export
data Modifier
    = JA_Public
    | JA_Private
    | JA_Protected
    | JA_Abstract
    | JA_Final
    | JA_Static
    | JA_StrictFP
    | JA_Transient
    | JA_Volatile
    | JA_Native
    | JA_Annotation Annotation
    | JA_Synchronized_

Show Modifier where
   show JA_Public = "public"
   show JA_Private = "private"
   show JA_Protected = "protected"
   show JA_Abstract = "abstract"
   show JA_Final = "final"
   show JA_Static = "static"
   show JA_StrictFP = "strictfp"
   show JA_Transient = "transient"
   show JA_Volatile = "volatile"
   show JA_Native = "native"
   show (JA_Annotation a) = "annotation" -- FIXME show a
   show JA_Synchronized_ = "synchronized"

------------------------------------------------------------------------------------------------------------------------

-- Forward declarations
data Exp : Type
data ClassBody : Type
data ArrayInit : Type
data FieldAccess : Type
data Block : Type

||| Explicit initializer for a variable declaration.
public export
data VarInit
    = JA_InitExp Exp
    | JA_InitArray ArrayInit

||| The name of a variable in a declaration, which may be an array.
public export
data VarDeclId : Type where
    JA_VarId : Ident -> VarDeclId
    ||| Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
    JA_VarDeclArray : VarDeclId -> VarDeclId

||| A declaration of a variable, which may be explicitly initialized.
public export
data VarDecl
    = JA_VarDecl VarDeclId (Maybe VarInit)

||| A formal parameter in method declaration. The last parameter
||| for a given declaration may be marked as variable arity,
||| indicated by the boolean argument.
public export
data FormalParam = JA_FormalParam (List Modifier) Type Bool VarDeclId

||| Arguments to methods and constructors are expressions.
export
Argument : Type
Argument = Exp

||| Array access
public export
data ArrayIndex = JA_ArrayIndex Exp (List Exp)    -- ^ Index into an array

||| The left-hand side of an assignment expression. This operand may be a named variable, such as a local
||| variable or a field of the current object or class, or it may be a computed variable, as can result from
||| a field access or an array access.
public export
data Lhs
    = JA_NameLhs Name          -- ^ Assign to a variable
    | JA_FieldLhs FieldAccess  -- ^ Assign through a field access
    | JA_ArrayLhs ArrayIndex   -- ^ Assign to an array

-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
public export
data LambdaParams
  = JA_LambdaSingleParam Ident
  | JA_LambdaFormalParams (List FormalParam)
  | JA_LambdaInferredParams (List Ident)

||| Lambda expression, starting from java 8
public export
data LambdaExpression
    = JA_LambdaExpression Exp
    | JA_LambdaBlock Block

||| A method invocation expression is used to invoke a class or instance method.
public export
data MethodInvocation : Type where
    ||| Invoking a specific named method.
    JA_MethodCall : Name -> (List Argument) -> MethodInvocation
    ||| Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    JA_PrimaryMethodCall : Exp -> (List RefType) -> Ident -> (List Argument) -> MethodInvocation
    ||| Invoking a method of the super class, giving arguments for any generic type parameters.
    JA_SuperMethodCall : (List RefType) -> Ident -> (List Argument) -> MethodInvocation
    ||| Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    JA_ClassMethodCall : Name -> (List RefType) -> Ident -> (List Argument) -> MethodInvocation
    ||| Invoking a method of a named type, giving arguments for any generic type parameters.
    JA_TypeMethodCall :  Name -> (List RefType) -> Ident -> (List Argument) -> MethodInvocation

||| A Java expression.
public export
data Exp : Type where
    ||| A literal denotes a fixed, unchanging value.
    JA_Lit : Literal -> Exp
    ||| A class literal, which is an expression consisting of the name of a class, interface, array,
    ||| or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    JA_ClassLit : Maybe Type -> Exp
    ||| The keyword @this@ denotes a value that is a reference to the object for which the instance method
    ||| was invoked, or to the object being constructed.
    JA_This : Exp
    ||| Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    JA_ThisClass : Name -> Exp
    ||| A class instance creation expression is used to create new objects that are instances of classes.
    ||| The first argument is a list of non-wildcard type arguments to a generic constructor.
    ||| What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    ||| optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    JA_InstanceCreation : (List TypeArgument) -> TypeDeclSpecifier -> (List Argument) -> (Maybe ClassBody) -> Exp
    ||| A qualified class instance creation expression enables the creation of instances of inner member classes
    ||| and their anonymous subclasses.
    JA_QualInstanceCreation : Exp -> (List TypeArgument) -> Ident -> (List Argument) -> (Maybe ClassBody) -> Exp
    ||| An array instance creation expression is used to create new arrays. The last argument denotes the number
    ||| of dimensions that have no explicit length given. These dimensions must be given last.
    JA_ArrayCreate : Type -> (List Exp) -> Int -> Exp
    ||| An array instance creation expression may come with an explicit initializer. Such expressions may not
    ||| be given explicit lengths for any of its dimensions.
    JA_ArrayCreateInit : Type -> Int -> ArrayInit -> Exp
    ||| A field access expression.
    JA_FieldAccess : FieldAccess -> Exp
    ||| A method invocation expression.
    JA_MethodInv : MethodInvocation -> Exp
    ||| An array access expression refers to a variable that is a component of an array.
    JA_ArrayAccess : ArrayIndex -> Exp
    ||| An expression name, e.g. a variable.
    JA_ExpName : Name -> Exp
    ||| Post-incrementation expression, i.e. an expression followed by @++@.
    JA_PostIncrement : Exp -> Exp
    ||| Post-decrementation expression, i.e. an expression followed by @--@.
    JA_PostDecrement : Exp -> Exp
    ||| Pre-incrementation expression, i.e. an expression preceded by @++@.
    JA_PreIncrement :  Exp -> Exp
    ||| Pre-decrementation expression, i.e. an expression preceded by @--@.
    JA_PreDecrement :  Exp -> Exp
    ||| Unary plus, the promotion of the value of the expression to a primitive numeric type.
    JA_PrePlus :  Exp -> Exp
    ||| Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    JA_PreMinus : Exp -> Exp
    ||| Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    JA_PreBitCompl : Exp -> Exp
    ||| Logical complementation of boolean values.
    JA_PreNot : Exp -> Exp
    ||| A cast expression converts, at run time, a value of one numeric type to a similar value of another
    ||| numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    ||| at run time, that a reference value refers to an object whose class is compatible with a specified
    ||| reference type.
    JA_Cast : Type -> Exp -> Exp
    ||| The application of a binary operator to two operand expressions.
    JA_BinOp : Exp -> Op -> Exp -> Exp
    ||| Testing whether the result of an expression is an instance of some reference type.
    JA_InstanceOf : Exp -> RefType -> Exp
    ||| The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    ||| expressions should be evaluated.
    JA_Cond : Exp -> Exp -> Exp -> Exp
    ||| Assignment of the result of an expression to a variable.
    JA_Assign : Lhs -> AssignOp -> Exp -> Exp
    ||| Lambda expression
    JA_Lambda : LambdaParams -> LambdaExpression -> Exp
    ||| Method reference
    JA_MethodRef : Name -> Ident -> Exp

||| A field access expression may access a field of an object or array, a reference to which is the value
||| of either an expression or the special keyword super.
public export
data FieldAccess
    = JA_PrimaryFieldAccess Exp Ident      -- ^ Accessing a field of an object or array computed from an expression.
    | JA_SuperFieldAccess Ident            -- ^ Accessing a field of the superclass.
    | JA_ClassFieldAccess Name Ident       -- ^ Accessing a (static) field of a named class.

------------------------------------------------------------------------------------------------------------------------
-- TODO




-- Forward declarations
data TypeDecl : Type
data ClassDecl : Type
data Stmt : Type
data BlockStmt : Type

-----------------------------------------------------------------------
-- Packages

||| A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
public export
data PackageDecl = JA_PackageDecl Name

||| An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
||| The first argument signals whether the declaration only imports static members.
||| The last argument signals whether the declaration brings all names in the named type or package, or only brings
||| a single name into scope.
public export
data ImportDecl
    = JA_ImportDecl Bool {- static? -} Name Bool {- .*? -}

||| A compilation unit is the top level syntactic goal symbol of a Java program.
public export
data CompilationUnit = JA_CompilationUnit (Maybe PackageDecl) (List ImportDecl) (List TypeDecl)

-----------------------------------------------------------------------
-- Declarations

-- Forward declarations
data Decl : Type
data InterfaceDecl : Type
data MemberDecl : Type

||| A type declaration declares a class type or an interface type.
public export
data TypeDecl
    = JA_ClassTypeDecl ClassDecl
    | JA_InterfaceTypeDecl InterfaceDecl

||| An enum constant defines an instance of the enum type.
public export
data EnumConstant = JA_EnumConstant Ident (List Argument) (Maybe ClassBody)

||| The body of an enum type may contain enum constants.
public export
data EnumBody = JA_EnumBody (List EnumConstant) (List Decl)

||| A class declaration specifies a new named reference type.
public export
data ClassDecl
    = JA_ClassDecl (List Modifier) Ident (List TypeParam) (Maybe RefType) (List RefType) ClassBody
    | JA_EnumDecl  (List Modifier) Ident                                  (List RefType) EnumBody

||| A class body may contain declarations of members of the class, that is,
||| fields, classes, interfaces and methods.
||| A class body may also contain instance initializers, static
||| initializers, and declarations of constructors for the class.
public export
data ClassBody = JA_ClassBody (List Decl)

||| The body of an interface may declare members of the interface.
public export
data InterfaceBody
    = JA_InterfaceBody (List MemberDecl)

||| Interface can declare either a normal interface or an annotation
public export
data InterfaceKind = JA_InterfaceNormal | JA_InterfaceAnnotation

||| An interface declaration introduces a new reference type whose members
||| are classes, interfaces, constants and abstract methods. This type has
||| no implementation, but otherwise unrelated classes can implement it by
||| providing implementations for its abstract methods.
public export
data InterfaceDecl
    = JA_InterfaceDecl InterfaceKind (List Modifier) Ident (List TypeParam) (List RefType) InterfaceBody

||| A declaration is either a member declaration, or a declaration of an
||| initializer, which may be static.
public export
data Decl
    = JA_MemberDecl MemberDecl
    | JA_InitDecl Bool Block

||| A method body is either a block of code that implements the method or simply a
||| semicolon, indicating the lack of an implementation (modelled by 'Nothing').
public export
data MethodBody = JA_MethodBody (Maybe Block)

||| An explicit constructor invocation invokes another constructor of the
||| same class, or a constructor of the direct superclass, which may
||| be qualified to explicitly specify the newly created object's immediately
||| enclosing instance.
public export
data ExplConstrInv
    = JA_ThisInvoke             (List RefType) (List Argument)
    | JA_SuperInvoke            (List RefType) (List Argument)
    | JA_PrimarySuperInvoke Exp (List RefType) (List Argument)

||| The first statement of a constructor body may be an explicit invocation of
||| another constructor of the same class or of the direct superclass.
public export
data ConstructorBody = JA_ConstructorBody (Maybe ExplConstrInv) (List BlockStmt)

||| A class or interface member can be an inner class or interface, a field or
||| constant, or a method or constructor. An interface may only have as members
||| constants (not fields), abstract methods, and no constructors.
public export
data MemberDecl : Type where
    ||| The variables of a class type are introduced by field declarations.
    JA_FieldDecl : (List Modifier) -> Type -> (List VarDecl) -> MemberDecl
    ||| A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    JA_MethodDecl :      (List Modifier) -> (List TypeParam) -> (Maybe Type) -> Ident -> (List FormalParam) -> (List ExceptionType) -> (Maybe Exp) -> MethodBody -> MemberDecl
    ||| A constructor is used in the creation of an object that is an instance of a class.
    JA_ConstructorDecl : (List Modifier) -> (List TypeParam)                 -> Ident -> (List FormalParam) -> (List ExceptionType) -> ConstructorBody -> MemberDecl
    ||| A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    JA_MemberClassDecl : ClassDecl -> MemberDecl
    ||| A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    JA_MemberInterfaceDecl : InterfaceDecl -> MemberDecl

||| Annotations may contain  annotations or (loosely) expressions
public export
data ElementValue
    = JA_EVVal VarInit
    | JA_EVAnn Annotation

record NormalAnnotation where
    constructor MkNormalAnnotation
    annName : Name -- Not type because not type generics not allowed
    annKV   : (List (Ident, ElementValue))

record SingleElementAnnotation where
    constructor MkSingleElementAnnotation
    annName : Name
    annValue: ElementValue

record MarkerAnnotation where
    constructor MkMarkerAnnotation
    annName : Name

||| Annotations have three different forms: no-parameter, single-parameter or key-value pairs
public export
data Annotation
    = JA_NormalAnnotation NormalAnnotation
    | JA_SingleElementAnnotation SingleElementAnnotation
    | JA_MarkerAnnotation MarkerAnnotation

-----------------------------------------------------------------------
-- Statements

||| A block is a sequence of statements, local class declarations
||| and local variable declaration statements within braces.
public export
data Block = JA_Block (List BlockStmt)

||| A block statement is either a normal statement, a local
||| class declaration or a local variable declaration.
public export
data BlockStmt
    = JA_BlockStmt Stmt
    | JA_LocalClass ClassDecl
    | JA_LocalVars (List Modifier) Type (List VarDecl)

||| If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
||| transferred to the first such catch clause.
public export
data Catch = JA_Catch FormalParam Block

||| A label within a @switch@ statement.
public export
data SwitchLabel : Type where
    ||| The expression contained in the @case@ must be a 'Lit' or an @enum@ constant.
    JA_SwitchCase : Exp -> SwitchLabel
    JA_Default : SwitchLabel

||| A block of code labelled with a @case@ or @default@ within a @switch@ statement.
public export
data SwitchBlock
    = JA_SwitchBlock SwitchLabel (List BlockStmt)

||| Initialization code for a basic @for@ statement.
public export
data ForInit
    = JA_ForLocalVars (List Modifier) Type (List VarDecl)
    | JA_ForInitExps (List Exp)

||| An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
||| array and providing some initial values
public export
data ArrayInit
    = JA_ArrayInit (List VarInit)

||| A Java statement.
public export
data Stmt : Type where
    ||| A statement can be a nested block.
    JA_StmtBlock : Block -> Stmt
    ||| The @if-then@ statement allows conditional execution of a statement.
    JA_IfThen : Exp -> Stmt -> Stmt
    ||| The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    JA_IfThenElse : Exp -> Stmt -> Stmt -> Stmt
    ||| The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    JA_While : Exp -> Stmt -> Stmt
    ||| The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    ||| update code repeatedly until the value of the expression is false.
    JA_BasicFor : (Maybe ForInit) -> (Maybe Exp) -> (Maybe (List Exp)) -> Stmt -> Stmt
    ||| The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    JA_EnhancedFor : (List Modifier) -> Type -> Ident -> Exp -> Stmt -> Stmt
    ||| An empty statement does nothing.
    JA_Empty : Stmt
    ||| Certain kinds of expressions may be used as statements by following them with semicolons:
    ||| assignments, pre- or post-inc- or decrementation, method invocation or class instance
    ||| creation expressions.
    JA_ExpStmt : Exp -> Stmt
    ||| An assertion is a statement containing a boolean expression, where an error is reported if the expression
    ||| evaluates to false.
    JA_Assert : Exp -> (Maybe Exp) -> Stmt
    ||| The switch statement transfers control to one of several statements depending on the value of an expression.
    JA_Switch : Exp -> (List SwitchBlock) -> Stmt
    ||| The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    JA_Do : Stmt -> Exp -> Stmt
    ||| A @break@ statement transfers control out of an enclosing statement.
    JA_Break : (Maybe Ident) -> Stmt
    ||| A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    ||| point of that statement.
    JA_Continue : (Maybe Ident) -> Stmt
    -- A @return@ statement returns control to the invoker of a method or constructor.
    JA_Return : (Maybe Exp) -> Stmt
    ||| A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    ||| then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    JA_Synchronized : Exp -> Block -> Stmt
    ||| A @throw@ statement causes an exception to be thrown.
    JA_Throw : Exp -> Stmt
    ||| A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    ||| can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    ||| clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    ||| and no matter whether a catch clause is first given control.
    JA_Try : Block -> (List Catch) -> (Maybe {- finally -} Block) -> Stmt
    ||| Statements may have label prefixes.
    JA_Labeled : Ident -> Stmt -> Stmt
