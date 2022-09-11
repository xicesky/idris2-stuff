
module Language.Java.Pretty

import Data.Maybe
import Data.List1
import Text.PrettyPrint.Bernardy
import Language.Java.AST.Types
import Language.Java.AST.Expression

%default total

------------------------------------------------------------------------------------------------------------------------
-- Helpers

export
vcatList : {opts : _} -> List (Doc opts) -> Doc opts
vcatList [] = empty
vcatList (x :: xs) = foldl1 vcat (x ::: xs)

export
hsepBy : {opts : _} -> Doc opts -> List (Doc opts) -> Doc opts
hsepBy _ [] = empty
hsepBy s (x :: xs) = foldl1 (hsepBy' s) (x ::: xs) <|> foldl1 vcat' (x ::: xs) where
    hsepBy' : Doc opts -> Doc opts -> Doc opts -> Doc opts
    hsepBy' s x y = hcat [x, s, y]
    vcat' : Doc opts -> Doc opts -> Doc opts
    vcat' x y = vcat x (s <+> y)

export
vsepBy : {opts : _} -> Doc opts -> List (Doc opts) -> Doc opts
vsepBy _ [] = empty
vsepBy s (x :: xs) = foldl1 vcat' (x ::: xs) where
    vcat' : Doc opts -> Doc opts -> Doc opts
    vcat' x y = vcat (vcat x s) y

export
indentDefault : {opts : _} -> Doc opts -> Doc opts
indentDefault = indent 4

export
vblock : {opts : _} -> Doc opts -> Doc opts -> Doc opts -> Doc opts
vblock top body bottom = vcatList [top, indentDefault body, bottom]

export
jbraces : {opts : _} -> Doc opts -> Doc opts -> Doc opts
jbraces header body = vblock (header `hsep` "{") body "}"

export
todo : {opts : _} -> String -> Doc opts
todo s = fromString $ "/* TODO: " ++ s ++ " */"

------------------------------------------------------------------------------------------------------------------------
-- Pretty-print stuff from Language.Java.AST.Types

Pretty Ident where
    pretty (JA_Ident s) = fromString s

Pretty Name where
    pretty (JA_Name idents) = hsepBy "." (map pretty idents)

Pretty PrimType where
    pretty JA_BooleanT = fromString "boolean"
    pretty JA_ByteT = fromString "byte"
    pretty JA_ShortT = fromString "short"
    pretty JA_IntT = fromString "int"
    pretty JA_LongT = fromString "long"
    pretty JA_CharT = fromString "char"
    pretty JA_FloatT = fromString "float"
    pretty JA_DoubleT = fromString "double"

Pretty JavaType where
    pretty (JA_PrimType t) = pretty t
    pretty (JA_RefType refT) = todo "JA_RefType"

------------------------------------------------------------------------------------------------------------------------

-- FIXME: I'd like forward declarations for instances...
-- ... instead we have to make this mutual...
mutual

Pretty Modifier where
    pretty JA_Public        = "public"
    pretty JA_Private       = "private"
    pretty JA_Protected     = "protected"
    pretty JA_Abstract      = "abstract"
    pretty JA_Final         = "final"
    pretty JA_Static        = "static"
    pretty JA_StrictFP      = "strictfp"
    pretty JA_Transient     = "transient"
    pretty JA_Volatile      = "volatile"
    pretty JA_Native        = "native"
    pretty (JA_Annotation an) = pretty an
    pretty JA_Synchronized_ = "synchronized"

Pretty (List Modifier) where
    pretty = hsepBy " " . map pretty

Pretty PackageDecl where
    pretty (JA_PackageDecl name) = "package" `hsep` (pretty name)

Pretty InterfaceDecl where
    -- JA_InterfaceDecl InterfaceKind (List Modifier) Ident (List TypeParam) (List RefType) InterfaceBody
    pretty (JA_InterfaceDecl _ _ _ _ _ _) = todo "InterfaceDecl"

Pretty ClassBody where
    pretty (JA_ClassBody decls) = vcatList $ map pretty decls

Pretty ClassDecl where
    pretty (JA_ClassDecl modifiers ident typeParams extendsT implementsTs classBody) = jbraces
        (sep [pretty modifiers, "class", pretty ident {- , FIXME tp,ext,impl -}])
        (pretty classBody)
    pretty (JA_EnumDecl  modifiers ident                     implementsTs enumBody) = todo "JA_EnumDecl"

Pretty TypeDecl where
    pretty (JA_ClassTypeDecl c) = pretty c
    pretty (JA_InterfaceTypeDecl i) = pretty i

Pretty CompilationUnit where
    pretty (JA_CompilationUnit packagedecl importdecls typedecls) = vsepBy empty
        [ fromMaybe empty (map pretty packagedecl)
        -- , FIXME importdecls
        , vsepBy empty (map pretty typedecls) --indentDefault $ vsepBy empty [fromString "impl"]
        ]

------------------------------------------------------------------------------------------------------------------------
-- Decl

Pretty VarDeclId where
    pretty (JA_VarId ident) = pretty ident
    pretty (JA_VarDeclArray vdi) = pretty vdi <+> "[]"

Pretty VarInit where
    pretty (JA_InitExp exp) = todo "JA_InitExp"
    pretty (JA_InitArray arrInit) = todo "JA_InitArray"

Pretty VarDecl where
    pretty (JA_VarDecl varDeclId maybeInit) = sep [pretty varDeclId, fromMaybe "" (map pretty maybeInit)]

Pretty MemberDecl where
    -- JA_FieldDecl : (List Modifier) -> JavaType -> (List VarDecl) -> MemberDecl
    pretty (JA_FieldDecl modifiers typ vardecl) = sep [pretty modifiers, pretty typ, hsepBy ", " $ map pretty vardecl] <+> ";"
    -- JA_MethodDecl :      (List Modifier) -> (List TypeParam) -> (Maybe Type) -> Ident -> (List FormalParam) -> (List ExceptionType) -> (Maybe Exp) -> MethodBody -> MemberDecl
    pretty (JA_MethodDecl _ _ _ _ _ _ _ _) = todo "JA_MethodDecl"
    -- JA_ConstructorDecl : (List Modifier) -> (List TypeParam)                 -> Ident -> (List FormalParam) -> (List ExceptionType) -> ConstructorBody -> MemberDecl
    pretty (JA_ConstructorDecl _ _ _ _ _ _) = todo "JA_ConstructorDecl"
    -- JA_MemberClassDecl : ClassDecl -> MemberDecl
    pretty (JA_MemberClassDecl _) = todo "JA_MemberClassDecl"
    -- JA_MemberInterfaceDecl : InterfaceDecl -> MemberDecl
    pretty (JA_MemberInterfaceDecl _) = todo "JA_MemberInterfaceDecl"

Pretty Decl where
    -- = JA_MemberDecl MemberDecl
    pretty (JA_MemberDecl memberDecl) = pretty memberDecl
    -- | JA_InitDecl Bool Block
    pretty (JA_InitDecl fixme block) = todo "JA_InitDecl"

------------------------------------------------------------------------------------------------------------------------

Pretty NormalAnnotation where
    pretty _ = todo "NormalAnnotation"

Pretty SingleElementAnnotation where
    pretty _ = todo "SingleElementAnnotation"

Pretty MarkerAnnotation where
    pretty (MkMarkerAnnotation name) = "@" <+> pretty name

Pretty Annotation where
    pretty (JA_NormalAnnotation x)          = pretty x
    pretty (JA_SingleElementAnnotation x)   = pretty x
    pretty (JA_MarkerAnnotation x)          = pretty x

------------------------------------------------------------------------------------------------------------------------

export
defaultOpts : LayoutOpts
defaultOpts = Opts 120

export
prettyPrint : CompilationUnit -> IO ()
prettyPrint cu = putStrLn $ fromMaybe "ERROR" $ Doc.render defaultOpts $ pretty $ cu

------------------------------------------------------------------------------------------------------------------------
-- debug only

-- render : (opts : _) -> Doc opts -> Maybe String
-- render opts (MkDoc xs) = map render $ shortest $ filter (visible opts) xs

Pretty String where
    pretty = text

export
doc : Doc Pretty.defaultOpts -> Doc Pretty.defaultOpts
doc = id

export
exampleDoc : Doc Pretty.defaultOpts
-- exampleDoc = vcat (indent 4 $ fromString "a") (fromString "b")
-- exampleDoc = "blah" <+> vcatList ["{", indentDefault "a", "}"]
-- exampleDoc = "a" <+> "b"
exampleDoc = (hsepBy ", " $ map pretty ["a", "b"]) <+> ";"

export
putdoc : Doc Pretty.defaultOpts -> IO ()
putdoc = putStrLn . fromMaybe "ERROR" . Doc.render defaultOpts
