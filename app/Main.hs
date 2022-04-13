module Main(main) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Vector
import Modify
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  handleArgs args

handleArgs :: [String] -> IO ()
-- if the first arg is "supports", then the second one will be the renderers name, do nothing & return exit status 0
handleArgs ("supports" : _) = return ()
-- otherwise load context & book json from stdin
handleArgs _ = do
  bs <- LB.getContents
  LB.putStr $ modifyJSON bs

modifyJSON :: LB.ByteString -> LB.ByteString
modifyJSON x = case (decode x :: Maybe Value) of
  Just (Array vals) -> encode $ modifyBook (vals ! 1) (vals ! 0)
  Just _ -> error "unexpected format"
  Nothing -> error "JSON parse error"
