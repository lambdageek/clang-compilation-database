module Main where

import Data.Aeson (eitherDecode')
import Data.Foldable (traverse_)
import qualified Data.ByteString.Lazy as B

import System.Exit (exitSuccess, exitFailure)

import Clang.CompilationDatabase

main :: IO ()
main = do
  let
    f = [ "test/smoke-test.json"
        , "test/autotool_project-compile_commands.json"
        ]
  (msgs, results) <- fmap unzip $ traverse smoketest f
  traverse_ putStrLn msgs
  if and results then exitSuccess else exitFailure

smoketest :: FilePath -> IO (String, Bool)
smoketest = \fp -> do
  b <- B.readFile fp
  let res = eitherDecode' b
  return $ report fp res
  where
    report :: FilePath -> Either String CompilationDatabase -> (String, Bool)
    report fp (Left err) = ("In " ++ fp ++ " got error: " ++ err, False)
    report fp (Right db) = ("In " ++ fp ++ " got " ++ show (length db) ++ " compile commands", True)

  
