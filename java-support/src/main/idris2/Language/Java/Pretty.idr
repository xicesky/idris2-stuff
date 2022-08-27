
module Language.Java.Pretty

import Data.Maybe
import Data.List1
import Text.PrettyPrint.Bernardy
import Language.Java.AST.Types
import Language.Java.AST.Expression

%default total

------------------------------------------------------------------------------------------------------------------------
-- Helpers

vcatList : {opts : _} -> List (Doc opts) -> Doc opts
vcatList [] = empty
vcatList (x :: xs) = foldl1 vcat (x ::: xs)

hsepBy : {opts : _} -> Doc opts -> List (Doc opts) -> Doc opts
hsepBy _ [] = empty
hsepBy s (x :: xs) = foldl1 (hsepBy' s) (x ::: xs) <|> foldl1 vcat' (x ::: xs) where
    hsepBy' : Doc opts -> Doc opts -> Doc opts -> Doc opts
    hsepBy' s x y = hcat [x, s, y]
    vcat' : Doc opts -> Doc opts -> Doc opts
    vcat' x y = vcat x (s <+> y)

vsepBy : {opts : _} -> Doc opts -> List (Doc opts) -> Doc opts
vsepBy _ [] = empty
vsepBy s (x :: xs) = foldl1 vcat' (x ::: xs) where
    vcat' : Doc opts -> Doc opts -> Doc opts
    vcat' x y = vcat (vcat x s) y

indentDefault : {opts : _} -> Doc opts -> Doc opts
indentDefault = indent 4

vblock : {opts : _} -> Doc opts -> Doc opts -> Doc opts -> Doc opts
vblock top body bottom = vcatList [top, indentDefault body, bottom]

jbraces : {opts : _} -> Doc opts -> Doc opts -> Doc opts
jbraces header body = vblock (header `hsep` "{") body "}"

todo : {opts : _} -> String -> Doc opts
todo s = fromString $ "// TODO " ++ s

------------------------------------------------------------------------------------------------------------------------
-- Pretty-print stuff from Language.Java.AST.Types

Pretty Ident where
    pretty (JA_Ident s) = fromString s

Pretty Name where
    pretty (JA_Name idents) = hsepBy "." (map pretty idents)

------------------------------------------------------------------------------------------------------------------------

Pretty PackageDecl where
    pretty (JA_PackageDecl name) = "package" `hsep` (pretty name)

Pretty InterfaceDecl where
    -- JA_InterfaceDecl InterfaceKind (List Modifier) Ident (List TypeParam) (List RefType) InterfaceBody
    pretty (JA_InterfaceDecl _ _ _ _ _ _) = todo "InterfaceDecl"

Pretty Decl where
    -- = JA_MemberDecl MemberDecl
    pretty (JA_MemberDecl memberDecl) = todo "JA_MemberDecl"
    -- | JA_InitDecl Bool Block
    pretty (JA_InitDecl fixme block) = todo "JA_InitDecl"

Pretty ClassBody where
    pretty (JA_ClassBody decls) = vcatList $ map pretty decls

Pretty ClassDecl where
    pretty (JA_ClassDecl modifiers ident typeParams extendsT implementsTs classBody) = jbraces
        (sep [{- FIXME modifiers, -} "class", pretty ident {- , FIXME tp,ext,impl -}])
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

export
doc : Doc Pretty.defaultOpts -> Doc Pretty.defaultOpts
doc = id

export
exampleDoc : Doc Pretty.defaultOpts
-- exampleDoc = vcat (indent 4 $ fromString "a") (fromString "b")
exampleDoc = "blah" <+> vcatList ["{", indentDefault "a", "}"]

export
putDoc : Doc Pretty.defaultOpts -> IO ()
putDoc = putStrLn . fromMaybe "ERROR" . Doc.render defaultOpts
