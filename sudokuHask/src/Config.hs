module Config where

import Data.Maybe

data Config = Config {
   inputFile :: String,
   outputFile :: Maybe String
  } deriving Show
