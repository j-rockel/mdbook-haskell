{-# LANGUAGE OverloadedStrings #-}

module Modify(
  modifyBook,
) where

import Control.Lens
import Data.Aeson
import Data.Aeson.KeyMap (member)
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.Vector as V
import FindDef
import RequestedSrc

-- | Main entrypoint
modifyBook :: Value -> Value -> Value
modifyBook book cxt = over (key "sections" . _Array) (V.map $ modifySection r) book
  where
    r = getProjectRoot cxt

modifySection :: Maybe FilePath -> Value -> Value
modifySection r v@(Object o)
  | member "Chapter" o =
      v
        & over
          (key "Chapter" . key "content" . _String)
          (replaceSrcRefs r)
        & over
          (key "Chapter" . key "sub_items" . _Array)
          (V.map $ modifySection r)
modifySection _ v = v

replaceSrcRefs :: Maybe FilePath -> T.Text -> T.Text
replaceSrcRefs r = T.unlines . map replaceSrcRef . T.lines
  where
    replaceSrcRef t =
      if T.isPrefixOf "{{#haskell-src" t
        then fulfillRequest $ parseRequestedSrc r t
        else t

getProjectRoot :: Value -> Maybe FilePath
getProjectRoot v = T.unpack . T.append (bookRootAbsolute <> "/") <$> haskellProjectRootRelative
  where
    haskellProjectRootRelative = v ^? key "config" . key "preprocessor" . key "haskell" . key "haskell-project-root" . _String
    bookRootAbsolute = v ^. key "root" . _String
