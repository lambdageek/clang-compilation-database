-- |
-- Module : Clang.CompilationDatabase
-- Description : JSON Compilation Database Format
-- License : MIT
-- Stability : experimental
--
-- Clang Compilation Database Format parser
--
-- The <http://clang.llvm.org/docs/JSONCompilationDatabase.html Clang Compilation Database Format> is a JSON
-- file format for recording compiler invocations in build systems.
--
--
{-# language DeriveGeneric #-}
module Clang.CompilationDatabase where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON{-, eitherDecode'-})
-- import qualified Data.ByteString.Lazy as B
import Data.Text (Text)

-- | A Compilation database consists of a sequence of 'CommandObject' values each of which is a
-- set of commands that acted upon a single source file.
type CompilationDatabase = [CommandObject]

-- | Each command object contains the translation unit’s main file, the working
-- directory of the compile run and the actual compile command.
data CommandObject = CommandObject {
  directory :: Text, -- ^ The working directory during compilation
  file :: Text, -- ^ The main translation unit processed by this compilation
                -- step.  There may be multiple 'CommandObject' values in a
                -- 'CompilationDatabase' for the same file if it was the main
                -- file for multiple compilation steps.
  command :: Maybe Text, -- ^ The command that was executed.  Double quotes and
                         -- backslashes are escaped by a backslash.
  arguments :: Maybe [Text], -- ^ The compile command executed as a list of
                             -- strings.  Either 'command' or 'arguments' is
                             -- required.  __TODO__ model this more faithfully.
  output :: Maybe Text -- ^ Optional name of the output file created by this
                       -- compilation step.
  }
  deriving (Generic, Show)

instance FromJSON CommandObject
instance ToJSON CommandObject

-- test :: FilePath -> IO (Either String [CommandObject])
-- test fp = fmap eitherDecode' (B.readFile fp)
