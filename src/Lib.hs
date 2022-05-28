module Lib
  ( someFunc,
  )
where

import qualified Data.Char
import qualified Data.List
import qualified Data.List.Split

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

someFunc :: IO ()
someFunc = putStrLn "someFunc"
