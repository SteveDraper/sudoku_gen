{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Solver where

import Data.Array.IArray
import qualified Data.Set as DS
import Data.Char(ord)
import Data.PQueue.Max as PQ
import Debug.Trace as DT

import Position

type Partition = DS.Set Char

data Partitions = Partitions {
  rows :: Array Int Partition,
  columns :: Array Int Partition,
  boxes :: Array (Int,Int) Partition
} deriving Show

data CellSets = CellSets {
  cell :: (Int, Int),
  rowSet :: Partition,
  columnSet :: Partition,
  boxSet :: Partition
} deriving Show

cellIndex CellSets{..} = cell

numChoices :: CellSets -> Int
numChoices cs = DS.size $ chosen cs

chosen :: CellSets -> DS.Set Char
chosen CellSets{..} = DS.union rowSet $ DS.union columnSet boxSet

type RemainingChoices = PQ.MaxQueue CellSets

instance Eq CellSets where
  rc1 == rc2 = (numChoices rc1) == (numChoices rc2)

instance Ord CellSets where
  compare rc1 rc2 = compare (numChoices rc1) (numChoices rc2)

data SolverState = SolverState Position Partitions RemainingChoices deriving Show

initialState :: Position -> SolverState
initialState pos@(Position cs p) = SolverState pos partitions (allChoices p partitions) where
  partitions = foldl accumulate emptyPartitions $ indices p
  emptyPartitions = Partitions lineArray lineArray boxArray where
    lineArray = listArray (0, dim pos - 1) $ replicate (dim pos) $ emptyPartition
    boxArray = listArray ((0,0), (baseDim-1, baseDim-1)) $ replicate (dim pos)  $ emptyPartition
    baseDim = case (dim pos) of
      4   -> 2
      9   -> 3
      16  -> 4
      25  -> 5
      _   -> undefined
  accumulate acc el = process $ p ! el where
    process Nothing = acc
    process (Just c) = update acc (fst el) (snd el) c
    update Partitions{..} row column value =
      Partitions (add rows row) (add columns column) (add boxes (boxIndices acc row column)) where
        add :: Ix i => Array i Partition -> i -> Array i Partition
        add ac i = ac//[(i, DS.insert value (ac!i))]
  emptyPartition = DS.empty

updateCell (SolverState pos partitions choices) (row, column) value =
  initialState newpos where
    newpos = setCell pos row column $ Just value

allChoices :: Array (Int, Int) (Maybe Char) -> Partitions -> RemainingChoices
allChoices cells partitions = foldl addCell empty allCells where
  addCell :: RemainingChoices -> (Int, Int) -> RemainingChoices
  addCell acc ix = cellChoices $ cells ! ix where
    cellChoices Nothing = PQ.insert (cellSets partitions ix) acc
    cellChoices (Just c) = acc
  allCells = indices cells

solve :: Position -> Maybe Position
solve p = toPosition <$> (doSolve $ initialState p) where
  toPosition (SolverState p _ _) = p
  doSolve s@(SolverState pos partitions choices)
    | PQ.null choices = if complete pos then Just s else Nothing
    | otherwise       = processMostConstrained where
    processMostConstrained = foldl first Nothing maybeSolutions
    first :: Maybe a -> Maybe a -> Maybe a
    first s@(Just _) _ = s
    first Nothing ms = ms
    maybeSolutions :: [Maybe SolverState]
    maybeSolutions =
      let cellSet = PQ.findMax choices
          valueChoices = DS.toList $ DS.difference (charSet pos) (chosen cellSet)
          cell = cellIndex cellSet
          choiceStates = (updateCell s cell) <$> valueChoices
      in doSolve <$> choiceStates

cellSets :: Partitions -> (Int, Int) -> CellSets
cellSets p@Partitions{..} (row, column) = CellSets (row, column) rowPartition columnPartition boxPartition where
  rowPartition = rows ! row
  columnPartition = columns ! column
  boxPartition = boxes ! (boxIndices p row column)

boxIndices p row column =
  let size = baseSize p
  in (quot row size, quot column size)

baseSize p@Partitions{..} =
  let b = bounds boxes
      upper = snd b
      lower = fst b
  in (fst upper) - (fst lower) + 1