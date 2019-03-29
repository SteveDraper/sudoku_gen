{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
import System.FilePath.Posix
import Data.Text as DT
import Data.Semigroup ((<>))
import System.Directory
import Data.Either

import Options.Applicative
import qualified Data.Text.IO as DTIO

import Config
import Position
import PositionSpecParser
import Solver


config :: Parser Config
config = Config
      <$> option str
          ( long "input" <>
            short 'i' <>
            metavar "Input file" <>
            help "Input File to read initial board state from" <>
            value "test4_solvable.txt"
          )
      <*> (optional $option str
          ( long "output" <>
            short 'o' <>
            metavar "Output file" <>
            help "Output file to write solution to"
          ))

main :: IO ()
main = execute =<< execParser opts where
          opts = info (config <**> helper)
           ( fullDesc
            <> progDesc "Solve generalized Sudoku (up to 25 X 25 currently"
            <> header "SudokuHask" )
          execute :: Config -> IO ()
          execute c@Config{..} = do
            initialPosSpec <- DTIO.readFile inputFile
            either reportError attemptSolve $ parsePosition initialPosSpec where
              reportError = hPutStrLn stderr
              attemptSolve p = maybe unsolveable (write . render) $ solve p
              write = writeOutput outputFile
              unsolveable = reportError "The provided position has no solutions"
              writeOutput Nothing =  putStrLn . DT.unpack
              writeOutput (Just file) = DTIO.writeFile file
