{-# LANGUAGE OverloadedStrings #-}

module FindDef(
  fulfillRequest,
) where

import Data.List (filter, findIndices)
import qualified Data.Text as T
import GHC.Data.EnumSet
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Hs
import GHC.Parser
import GHC.Parser.Lexer
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import RequestedSrc
import System.Directory
import System.IO.Unsafe
import GHC.LanguageExtensions (Extension(..))
import GHC.Utils.Error (DiagOpts(..))
import GHC.Utils.Outputable (defaultSDocContext)
import GHC.Unit.Module.Warnings (emptyWarningCategorySet)

fulfillRequest :: RequestedSrc -> T.Text
fulfillRequest (RequestedSrc filename x format) =
  let parsedModule = parseFileIfExists filename
      decls = findDecl x format parsedModule
   in case decls of
        [] -> error $ "No declaration found for " <> T.unpack x
        _ -> formatAsImport filename $ defSpan decls

formatAsImport :: FilePath -> SrcSpan -> T.Text
formatAsImport fp (RealSrcSpan s _) = asHaskellCodeblock $ "{{#include " <> T.pack fp <> ":" <> start <> ":" <> end <> "}}"
  where
    asHaskellCodeblock :: T.Text -> T.Text
    asHaskellCodeblock t = "```haskell\n" <> t <> "\n```"
    start = T.pack $ show $ srcSpanStartLine s
    end = T.pack $ show $ srcSpanEndLine s

parseFileIfExists :: FilePath -> Located (HsModule GhcPs)
parseFileIfExists filename = unsafePerformIO $ do
  exists <- doesFileExist filename
  if exists then parse filename else error $ "File " ++ filename ++ " does not exist"

parse :: FilePath -> IO (Located (HsModule GhcPs))
parse filename = do
  buffer <- hGetStringBuffer filename
  let location = mkRealSrcLoc (mkFastString filename) 1 1
      extensions = fromList [QuasiQuotes, TemplateHaskell, OverloadedStrings, OverloadedRecordDot]
      diagOpts = DiagOpts
        { diag_warning_flags = empty
        , diag_fatal_warning_flags = empty
        , diag_warn_is_error = False
        , diag_reverse_errors = False
        , diag_max_errors = Nothing
        , diag_ppr_ctx = defaultSDocContext
        , diag_custom_warning_categories = emptyWarningCategorySet
        , diag_fatal_custom_warning_categories = emptyWarningCategorySet
        }
      parserOpts = mkParserOpts extensions diagOpts [] False True True True
      parseState = initParserState parserOpts buffer location
   in case unP parseModule parseState of
        POk _ lm -> return lm
        PFailed _ -> error $ "Parse failed for " ++ filename

findDecl :: T.Text -> Format -> Located (HsModule GhcPs) -> [LHsDecl GhcPs]
findDecl name format (L _ (HsModule{hsmodDecls = decls})) = getDeclsAndComments $ findIndices (defines n format) decls
  where
    n = T.unpack name
    getDeclsAndComments [] = []
    getDeclsAndComments (i : is) = declWithComments i decls ++ getDeclsAndComments is
    declWithComments i decls =
      let before = if i > 0 then filter isCommentBefore [decls !! (i - 1)] else []
          after = if i < length decls - 1 then filter isCommentAfter [decls !! (i + 1)] else []
       in before ++ [decls !! i] ++ after

isCommentBefore :: LHsDecl GhcPs -> Bool
isCommentBefore (L _ (DocD _ (DocCommentNext _))) = True
isCommentBefore _ = False

isCommentAfter :: LHsDecl GhcPs -> Bool
isCommentAfter (L _ (DocD _ (DocCommentPrev _))) = True
isCommentAfter _ = False

defines :: String -> Format -> LHsDecl GhcPs -> Bool
defines name _ (L _ (TyClD _ (SynDecl _ id _ _ _))) = idIsName name id
defines name _ (L _ (TyClD _ (DataDecl _ id _ _ _))) = idIsName name id
defines name _ (L _ (TyClD _ (ClassDecl{tcdLName = id}))) = idIsName name id
defines name _ (L _ (SigD _ (TypeSig _ ids _))) = any (idIsName name) ids
defines name _ (L _ (SigD _ (PatSynSig _ ids _))) = any (idIsName name) ids
defines name _ (L _ (SigD _ (ClassOpSig _ _ ids _))) = any (idIsName name) ids
defines name _ (L _ (SigD _ (FixSig _ (FixitySig _ ids _)))) = any (idIsName name) ids
defines name Full (L _ (ValD _ (FunBind{fun_id = id}))) = idIsName name id
defines name Full (L _ (ValD _ (VarBind _ x _))) = isName name x
defines _ _ _ = False

idIsName :: String -> LIdP GhcPs -> Bool
idIsName name (L _ x) = isName name x

isName :: String -> RdrName -> Bool
isName name (Unqual n) = occNameString n == name
isName name (Qual _ n) = occNameString n == name
isName name (Orig _ n) = occNameString n == name
isName name (Exact n) = occNameString (nameOccName n) == name

defSpan :: [GenLocated SrcSpanAnnA a] -> SrcSpan
defSpan = foldr1 combineSrcSpans . map getLocA
