module Position where

import Data.Array.IArray
import Data.Maybe
import Data.Set(Set)

type CharSet = Set Char
data Position = Position CharSet (Array (Int, Int) (Maybe Char)) deriving Show

charSet (Position cs _) = cs

getCell (Position _ p) row column = p ! (row, column)

setCell (Position cs p) row column value = Position cs $ p // [((row, column), value)]

dim (Position _ p) =
  let b = bounds p
      upper = snd b
      lower = fst b
  in (fst upper) - (fst lower) + 1

complete (Position _ p) = all isJust $ elems p