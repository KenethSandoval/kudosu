module Lib
  ( someFunc,
  )
where

import qualified Control.Monad
import qualified Data.Char
import Data.Foldable (Foldable (length), concatMap)
import Data.Function
import Data.List (genericIndex)
import qualified Data.List
import qualified Data.List.Split
import Data.Maybe
import Data.Traversable (Traversable (traverse))
import Prelude (getContents, putStr, putStrLn)

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)

type Row = [Cell]

type Grid = [Row]

readGrid :: String -> Maybe Grid
readGrid s
  | length s == 81 = traverse (traverse readCell) . Data.List.Split.chunksOf 9 $ s
  | otherwise = Nothing
  where
    readCell '.' = Just $ Possible [1 .. 9]
    readCell c
      | Data.Char.isDigit c && c > '0' = Just . Fixed . Data.Char.digitToInt $ c
      | otherwise = Nothing

showGrid :: Grid -> String
showGrid = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x) = show x
    showCell _ = "."

showGridWithPossibilities :: Grid -> String
showGridWithPossibilities = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x) = show x ++ "     "
    showCell (Possible xs) =
      (++ "]")
        . Data.List.foldl' (\acc x -> acc ++ if x `elem` xs then show x else " ") "["
        $ [1 .. 9]

pruneCells :: [Cell] -> Maybe [Cell]
pruneCells cells = traverse pruneCells cells
  where
    fixeds = [x | Fixed x <- cells]

    pruneCells (Possible xs) = case xs Data.List.\\ fixeds of
      [] -> Nothing
      [y] -> Just $ Fixed y
      ys -> Just $ Possible ys
    pruneCells x = Just x

subGridsToRow :: Grid -> Grid
subGridsToRow =
  concatMap
    ( \rows ->
        let [r1, r2, r3] = map (Data.List.Split.chunksOf 3) rows
         in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3
    )
    . Data.List.Split.chunksOf 3

pruneGrid' :: Grid -> Maybe Grid
pruneGrid' grid =
  traverse pruneCells grid
    >>= fmap Data.List.transpose . traverse pruneCells . Data.List.transpose
    >>= fmap subGridsToRow . traverse pruneCells . subGridsToRow

pruneGrid :: Grid -> Maybe Grid
pruneGrid = fixM pruneGrid'
  where
    fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'

nextGrids :: Grid -> (Grid, Grid)
nextGrids grid =
  let (i, first@(Fixed _), rest) =
        fixCell
          . Data.List.minimumBy (compare `Data.Function.on` (possibilityCount . snd))
          . filter (isPossible . snd)
          . zip [0 ..]
          . concat
          $ grid
   in (replace2D i first grid, replace2D i rest grid)
  where
    isPossible (Possible _) = True
    isPossible _ = False

    possibilityCount (Possible xs) = length xs
    possibilityCount (Fixed _) = 1

    fixCell (i, Possible [x, y]) = (i, Fixed x, Fixed y)
    fixCell (i, Possible (x : xs)) = (i, Fixed x, Possible xs)
    fixCell _ = error "Impossible case"

    replace2D :: Int -> a -> [[a]] -> [[a]]
    replace2D i v =
      let (x, y) = (i `quot` 9, i `mod` 9) in replace x (replace y (const v))
    replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0 ..]]

isGridFilled :: Grid -> Bool
isGridFilled grid = null [() | Possible _ <- concat grid]

isGridInvalid :: Grid -> Bool
isGridInvalid grid =
  any isInvalidRow grid
    || any isInvalidRow (Data.List.transpose grid)
    || any isInvalidRow (subGridsToRow grid)
  where
    isInvalidRow row =
      let fixeds = [x | Fixed x <- row]
          emptyPossibles = [x | Possible x <- row, null x]
       in hasDups fixeds || not (null emptyPossibles)

    hasDups l = hasDups' l []

    hasDups' [] _ = False
    hasDups' (y : ys) xs
      | y `elem` xs = True
      | otherwise = hasDups' ys (y : xs)

solve :: Grid -> Maybe Grid
solve grid = pruneGrid grid >>= solve'
  where
    solve' g
      | isGridInvalid g = Nothing
      | isGridFilled g = Just g
      | otherwise =
        let (grid1, grid2) = nextGrids g
         in solve grid1 <> solve grid2

someFunc :: IO ()
someFunc = do
  inputs <= lines <$> getContents
  Control.Monad.forM_ inputs $ \input ->
    case readGrid input of
      Nothing -> putStr "Invalid input"
      Just grid -> case solve grid of
        Nothing -> putStrLn "No solution found"
        Just grind' -> putStrLn $ showGrid grid'
