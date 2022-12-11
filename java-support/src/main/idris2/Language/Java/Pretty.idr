
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

implementation Pretty Ident where
    prettyPrec _ (JA_Ident s) = fromString s

implementation Pretty Name where
    prettyPrec _ (JA_Name idents) = hsepBy "." (map (prettyPrec App) idents)

implementation Pretty PrimType where
    prettyPrec _ JA_BooleanT = fromString "boolean"
    prettyPrec _ JA_ByteT = fromString "byte"
    prettyPrec _ JA_ShortT = fromString "short"
    prettyPrec _ JA_IntT = fromString "int"
    prettyPrec _ JA_LongT = fromString "long"
    prettyPrec _ JA_CharT = fromString "char"
    prettyPrec _ JA_FloatT = fromString "float"
    prettyPrec _ JA_DoubleT = fromString "double"

implementation Pretty JavaType where
    prettyPrec _ (JA_PrimType t) = prettyPrec App t
    prettyPrec _ (JA_RefType refT) = todo "JA_RefType"

------------------------------------------------------------------------------------------------------------------------

implementation Pretty Annotation
implementation Pretty Decl

implementation Pretty Modifier where
    prettyPrec _ JA_Public        = "public"
    prettyPrec _ JA_Private       = "private"
    prettyPrec _ JA_Protected     = "protected"
    prettyPrec _ JA_Abstract      = "abstract"
    prettyPrec _ JA_Final         = "final"
    prettyPrec _ JA_Static        = "static"
    prettyPrec _ JA_StrictFP      = "strictfp"
    prettyPrec _ JA_Transient     = "transient"
    prettyPrec _ JA_Volatile      = "volatile"
    prettyPrec _ JA_Native        = "native"
    prettyPrec _ (JA_Annotation an) = prettyPrec App an
    prettyPrec _ JA_Synchronized_ = "synchronized"

isAnnotation : Modifier -> Bool
isAnnotation (JA_Annotation _)  = True
isAnnotation _                  = False

prettyModifiers : {opts : _} -> Prec -> List Modifier -> Doc opts
--prettyModifiers _ = hsepBy " " . map pretty
-- FIXME is the order of modifiers important?? I don't think so
prettyModifiers _ modifiers = let
        annotations : List Modifier
        annotations = filter isAnnotation modifiers
        nonAnnotations : List Modifier
        nonAnnotations = filter (not . isAnnotation) modifiers
    in vcat (hsepBy " " $ map pretty annotations) (hsepBy " " $ map pretty nonAnnotations)

implementation Pretty PackageDecl where
    prettyPrec _ (JA_PackageDecl name) = "package" `hsep` ((prettyPrec App) name)

implementation Pretty InterfaceDecl where
    -- JA_InterfaceDecl InterfaceKind (List Modifier) Ident (List TypeParam) (List RefType) InterfaceBody
    prettyPrec _ (JA_InterfaceDecl _ _ _ _ _ _) = todo "InterfaceDecl"

implementation Pretty ClassBody where
    prettyPrec _ (JA_ClassBody decls) = vcatList $ map (prettyPrec App) decls

implementation Pretty ClassDecl where
    prettyPrec _ (JA_ClassDecl modifiers ident typeParams extendsT implementsTs classBody) = jbraces
        (sep [prettyModifiers App modifiers, "class", prettyPrec App ident {- , FIXME tp,ext,impl -}])
        (prettyPrec App classBody)
    prettyPrec _ (JA_EnumDecl  modifiers ident                     implementsTs enumBody) = todo "JA_EnumDecl"

implementation Pretty TypeDecl where
    prettyPrec _ (JA_ClassTypeDecl c) = prettyPrec App c
    prettyPrec _ (JA_InterfaceTypeDecl i) = prettyPrec App i

implementation Pretty CompilationUnit where
    prettyPrec _ (JA_CompilationUnit packagedecl importdecls typedecls) = vsepBy empty
        [ fromMaybe empty (map (prettyPrec App) packagedecl)
        -- , FIXME importdecls
        , vsepBy empty (map (prettyPrec App) typedecls) --indentDefault $ vsepBy empty [fromString "impl"]
        ]

------------------------------------------------------------------------------------------------------------------------
-- Decl

implementation Pretty VarDeclId where
    prettyPrec _ (JA_VarId ident) = prettyPrec App ident
    prettyPrec _ (JA_VarDeclArray vdi) = prettyPrec App vdi <+> "[]"

implementation Pretty VarInit where
    prettyPrec _ (JA_InitExp exp) = todo "JA_InitExp"
    prettyPrec _ (JA_InitArray arrInit) = todo "JA_InitArray"

implementation Pretty VarDecl where
    prettyPrec _ (JA_VarDecl varDeclId maybeInit) = sep [prettyPrec App varDeclId, fromMaybe "" (map (prettyPrec App) maybeInit)]

implementation Pretty MemberDecl where
    -- JA_FieldDecl : (List Modifier) -> JavaType -> (List VarDecl) -> MemberDecl
    prettyPrec _ (JA_FieldDecl modifiers typ vardecl) = sep [prettyModifiers App modifiers, prettyPrec App typ, hsepBy ", " $ map (prettyPrec App) vardecl] <+> ";"
    -- JA_MethodDecl :      (List Modifier) -> (List TypeParam) -> (Maybe Type) -> Ident -> (List FormalParam) -> (List ExceptionType) -> (Maybe Exp) -> MethodBody -> MemberDecl
    prettyPrec _ (JA_MethodDecl _ _ _ _ _ _ _ _) = todo "JA_MethodDecl"
    -- JA_ConstructorDecl : (List Modifier) -> (List TypeParam)                 -> Ident -> (List FormalParam) -> (List ExceptionType) -> ConstructorBody -> MemberDecl
    prettyPrec _ (JA_ConstructorDecl _ _ _ _ _ _) = todo "JA_ConstructorDecl"
    -- JA_MemberClassDecl : ClassDecl -> MemberDecl
    prettyPrec _ (JA_MemberClassDecl _) = todo "JA_MemberClassDecl"
    -- JA_MemberInterfaceDecl : InterfaceDecl -> MemberDecl
    prettyPrec _ (JA_MemberInterfaceDecl _) = todo "JA_MemberInterfaceDecl"

implementation Pretty Decl where
    -- = JA_MemberDecl MemberDecl
    prettyPrec _ (JA_MemberDecl memberDecl) = prettyPrec App memberDecl
    -- | JA_InitDecl Bool Block
    prettyPrec _ (JA_InitDecl fixme block) = todo "JA_InitDecl"

------------------------------------------------------------------------------------------------------------------------

implementation Pretty NormalAnnotation where
    prettyPrec _ _ = todo "NormalAnnotation"

implementation Pretty SingleElementAnnotation where
    prettyPrec _ _ = todo "SingleElementAnnotation"

implementation Pretty MarkerAnnotation where
    prettyPrec _ (MkMarkerAnnotation name) = "@" <+> prettyPrec App name

implementation Pretty Annotation where
    prettyPrec _ (JA_NormalAnnotation x)          = prettyPrec App x
    prettyPrec _ (JA_SingleElementAnnotation x)   = prettyPrec App x
    prettyPrec _ (JA_MarkerAnnotation x)          = prettyPrec App x

------------------------------------------------------------------------------------------------------------------------

export
defaultOpts : LayoutOpts
defaultOpts = Opts 120

export
prettyPrint : CompilationUnit -> IO ()
prettyPrint cu = putStrLn $ Doc.render defaultOpts $ prettyPrec App $ cu

------------------------------------------------------------------------------------------------------------------------
-- debug only

-- render : (opts : _) -> Doc opts -> Maybe String
-- render opts (MkDoc xs) = map render $ shortest $ filter (visible opts) xs

export
doc : Doc Pretty.defaultOpts -> Doc Pretty.defaultOpts
doc = id

export
exampleDoc : Doc Pretty.defaultOpts
-- exampleDoc = vcat (indent 4 $ fromString "a") (fromString "b")
-- exampleDoc = "blah" <+> vcatList ["{", indentDefault "a", "}"]
-- exampleDoc = "a" <+> "b"
exampleDoc = (hsepBy ", " $ map (prettyPrec App) ["a", "b"]) <+> ";"

export
putdoc : Doc Pretty.defaultOpts -> IO ()
putdoc = putStrLn . Doc.render defaultOpts
