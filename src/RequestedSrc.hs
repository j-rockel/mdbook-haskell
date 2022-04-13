{-# LANGUAGE OverloadedStrings #-}

module RequestedSrc(
  RequestedSrc(..),
  parseRequestedSrc
) where

import qualified Data.Text as T

data RequestedSrc = RequestedSrc
  { filepath :: FilePath,
    name :: T.Text
  }

parseRequestedSrc :: Maybe FilePath -> T.Text -> RequestedSrc
parseRequestedSrc maybePathToRoot request =
  let requestWords = T.words request
   in case tail requestWords of
        [filepath, name] -> RequestedSrc (pathToRoot ++ T.unpack filepath) (T.dropEnd 2 name)
        _ -> parseError request
  where
    pathToRoot = case maybePathToRoot of
      Just path -> path ++ "/"
      Nothing -> ""
    parseError :: T.Text -> RequestedSrc
    parseError r = error $ "Syntax error in haskell-src request: " ++ T.unpack r ++ "\nRequest must be of the form: \"{{#haskell-src <filepath> <name>}}\""
