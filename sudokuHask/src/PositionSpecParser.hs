{-# LANGUAGE OverloadedStrings #-}

module PositionSpecParser where

import Position
import Data.Text(Text, pack, unpack)
import Data.List(intercalate, intersperse, nub)
import Data.Either
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Control.Monad
import Data.Either.Combinators
import Data.Array.IArray
import qualified Data.Set as DS

type SimpleParser a = GenParser Char () a

parsePosition :: Text -> Either String Position
parsePosition = (doParse >=> validateBoard) . unpack where
  doParse = (mapLeft show) . (parse board "Initial")
  validateBoard ll =
    let len = length ll
        charSet = nub $ filter (\c -> c /= '.') $ concat ll
    in if validLen(len) && length charSet == len && all (\lc -> length lc == len) ll
      then Right $ stringsToPosition len ll
      else Left "Invalid initial position"
  validLen n = (n == 4) || (n == 9) || (n == 16) || (n == 25)
  toPosition ll = undefined

stringsToPosition :: Int -> [String] -> Position
stringsToPosition n ll = Position charSet $ (listArray ((0,0), (n-1,n-1))) . map toCell $ concat ll where
  toCell '.' = Nothing
  toCell c = Just c
  charSet = DS.fromList $ nub $ filter (\c -> c /= '.') $ concat ll

render :: Position -> Text
render p = pack $ intercalate "\n" $ rows where
  rows = map getRow [0..size-1]
  getRow row = intersperse ' ' $ (cellChar row) <$> [0..size-1]
  cellChar row column = toChar $ getCell p row column
  toChar Nothing = '.'
  toChar (Just c) = c
  size = dim p

cell = alphaNum <|> (char '.')
line :: SimpleParser [Char]
line = many1 cell <* endOfLine
board = many1 line