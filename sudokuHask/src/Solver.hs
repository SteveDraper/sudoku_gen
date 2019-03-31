{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Solver where

import Data.Array.IArray
import qualified Data.Set as DS
import Data.Char(ord)
import Debug.Trace as DT
import Data.Foldable as F

import Position

-- solver cell state is the set of remaining choices plus a boolean indicating the selection has been fixed
type SolverCellState = (DS.Set Char, Bool)
data SolverState = SolverState CellIndices (Position SolverCellState)

-- Cached data structure containing for each cell position the list of cell positions
-- it shares a line or box with
type CellIndices = Array (Int, Int) (DS.Set (Int, Int))

makeCellIndices :: Int -> CellIndices
makeCellIndices n = listArray bounds dependencies where
  bounds = ((0,0), (n-1,n-1))
  dependencies = fmap dependents $ range bounds
  dependents (r, c) = DS.difference allSharedPartition self where
    allSharedPartition = DS.unions (row : column : box : [])
    self = DS.singleton (r, c)
    row = DS.fromList $ rowCellIndices n r
    column = DS.fromList $ columnCellIndices n c
    box = DS.fromList $ boxCellIndices n r c

dependentCells :: CellIndices -> (Int, Int) -> DS.Set (Int, Int)
dependentCells ci idx = ci ! idx

-- Solver state uses positions encoded by the set of remaining choices for each cell
toSolverPos :: Position (Maybe Char) -> Position SolverCellState
toSolverPos (Position cs p) = Position cs remainingChoices where
  remainingChoices = listArray (bounds p) indexChoices
  indexChoices = fmap calculateRemaining (indices p)
  calculateRemaining (row, column) = convertCell $ p ! (row, column) where
    convertCell (Just c) = (DS.singleton c, True)
    convertCell Nothing = (DS.difference (DS.fromList cs) disallowed, False)
    disallowed = DS.unions (hline : vline : box : [])
    hline = usedSet hLineCells
    hLineCells = cells $ rowCellIndices (dim p) row
    vline = usedSet vLineCells
    vLineCells = cells $ columnCellIndices (dim p) column
    box = usedSet boxCells
    boxCells = cells $ boxCellIndices (dim p) row column
    cells = fmap (p !)
    usedSet lc = foldl addCell DS.empty lc where
      addCell s (Just c) = DS.insert c s
      addCell s Nothing = s

initialState :: (Position (Maybe Char)) -> SolverState
initialState pos@(Position _ p) = SolverState (makeCellIndices (dim p)) $ toSolverPos pos

updateCell ci (Position cs p) idx value = Position cs p' where
  p' =
    let updateIndices = dependentCells ci idx
        updatedValue :: ((Int, Int), (DS.Set Char, Bool)) -> (DS.Set Char, Bool)
        updatedValue (i, oldVal)         | i == idx  = (DS.singleton value, True)
        updatedValue (_, oldVal@(_, True))           = oldVal
        updatedValue (i, oldVal)         | not $ DS.member i updateIndices = oldVal
        updatedValue (_, (allowed, _))   | otherwise = (DS.delete value allowed, False)
    in
        listArray (bounds p) (fmap updatedValue $ assocs p)

solve :: (Position (Maybe Char)) -> (Int, Maybe (Position (Maybe Char)))
solve p = (count, position) where
  (count, solutionState) = doSolve $ initialState p
  position = toPosition <$> solutionState
  toPosition (SolverState _ (Position cs p)) = Position cs $ fmap (DS.lookupMin . fst) p
  doSolve :: SolverState -> (Int, Maybe SolverState)
  doSolve ss@(SolverState ci s@(Position _ choices)) =
    let numChoices (idx, cellState) = (idx, DS.size (fst cellState))
        openChoices = filter (not . snd . snd) $ assocs choices
        choiceSizes = fmap numChoices openChoices
        idx = fst $ lowest ((0,0), 100) choiceSizes where
          lowest acc [] = acc
          lowest acc@(i, 0) _ = acc
          lowest (_, acc_n) (el@(i, n) : r) | n < acc_n = lowest el r
          lowest acc (h : r) | otherwise = lowest acc r
        search = solveChoice $ fst $ choices ! idx
        solveChoice available | DS.size available == 0 = (1, Nothing)
        solveChoice available | otherwise = foldl first (0, Nothing) $ maybeSolutions $ DS.toList available
        first (m, s@(Just _)) _ = (m, s)
        first (m, Nothing) (m2, x) = (m + m2, x)
        maybeSolutions cs =
          let choiceStates = (updateCell ci s idx) <$> cs
          in (doSolve . (\p -> SolverState ci p)) <$> choiceStates
    in if choiceSizes == [] then (1, Just ss) else search

rowCellIndices :: Int -> Int -> [(Int, Int)]
rowCellIndices n row = fmap (\i -> (row, i)) [0..n-1]

columnCellIndices :: Int -> Int -> [(Int, Int)]
columnCellIndices n column = fmap (\i -> (i, column)) [0..n-1]

boxCellIndices :: Int -> Int -> Int -> [(Int, Int)]
boxCellIndices n row column = filter inBox $ range ((0,0), (n-1,n-1)) where
  inBox (r,c) = ((r `quot` baseSize) == (row `quot` baseSize)) && ((c `quot` baseSize) == (column `quot` baseSize))
  baseSize = case n of
    4   -> 2
    9   -> 3
    16  -> 4
    25  -> 5
    _   -> undefined
