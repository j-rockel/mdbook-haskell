{-# LANGUAGE OverloadedStrings #-}

module RequestedSrc(
  RequestedSrc(..),
  parseRequestedSrc,
  Format(..)
) where

import qualified Data.Text as T

data Format = Full | Signature

data RequestedSrc = RequestedSrc
  { filepath :: FilePath,
    name :: T.Text,
    format :: Format
  }

parseRequestedSrc :: Maybe FilePath -> T.Text -> RequestedSrc
parseRequestedSrc maybePathToRoot request =
  let requestWords = T.words request
   in case tail requestWords of
        [filepath, name] -> RequestedSrc (pathToRoot ++ T.unpack filepath) (T.dropEnd 2 name) Full
        [filepath, name, format] -> RequestedSrc (pathToRoot ++ T.unpack filepath) name (parseFormat $ T.dropEnd 2 format)
        _ -> parseError request
  where
    pathToRoot = case maybePathToRoot of
      Just path -> path ++ "/"
      Nothing -> ""
    parseFormat :: T.Text -> Format
    parseFormat "full" =  Full
    parseFormat "signature" = Signature
    parseFormat f = error $ "Syntax error in haskell-src request: Unknown format specified: " ++ T.unpack f ++ "\nAvailable format arguments are 'full' or 'signature'"
    parseError :: T.Text -> RequestedSrc
    parseError r = error $ "Syntax error in haskell-src request: " ++ T.unpack r ++ "\nRequest must be of the form: \"{{#haskell-src <filepath> <name> <format>}}\""
