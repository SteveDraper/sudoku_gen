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
import System.CPUTime

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
            value "test9_solvable.txt"
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
            startTime <- getCPUTime
            either reportError attemptSolve $ parsePosition initialPosSpec
            endTime <- getCPUTime
            putStrLn $ "Total time: " <> (show $ executionTime startTime endTime) <> "s" where
              reportError = hPutStrLn stderr
              attemptSolve p = handle $ solve p where
                handle (n, Nothing) = unsolveable n
                handle (n, Just pos) = ((write . render) pos) >> (putStrLn $ "Solution visited " <> (show n) <> " states")
                write = writeOutput outputFile
                unsolveable n = reportError $ "The provided position has no solutions (" <> (show n) <> " states visited)"
                writeOutput Nothing =  putStrLn . DT.unpack
                writeOutput (Just file) = DTIO.writeFile file
              executionTime start end  = ((fromIntegral (end - start)) / (10^12) :: Double)
