module Position where

import Data.Array.IArray
import Data.Maybe

type CharSet = [Char]
data Position a = Position CharSet (Array (Int, Int) a) deriving Show

charSet (Position cs _) = cs

getCell (Position _ p) row column = p ! (row, column)

setCell (Position cs p) row column value = Position cs $ p // [((row, column), value)]

dim pa =
  let b = bounds pa
      upper = snd b
      lower = fst b
  in (fst upper) - (fst lower) + 1

rowIndices pa =
  let b = bounds pa
      upper = snd b
      lower = fst b
  in [(fst lower)..(fst upper)]

columnIndices pa =
  let b = bounds pa
      upper = snd b
      lower = fst b
  in [(snd lower)..(snd upper)]

