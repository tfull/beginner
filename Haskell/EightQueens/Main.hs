module Main where

import qualified Control.Monad as M
import qualified Data.List as L

size :: Int
size = 8

getSomeLines :: Int -> IO [String]
getSomeLines n
    | n == 0 = return []
    | otherwise = do
        hd <- getLine
        tl <- getSomeLines $ n - 1
        return $ hd : tl

readBoard :: IO [[Char]]
readBoard = getSomeLines size

boardToList :: [[Char]] -> [(Int, Int)]
boardToList = withIndex (0, 0)
    where
        withIndex :: (Int, Int) -> [[Char]] -> [(Int, Int)]
        withIndex _ [] = []
        withIndex (i, _) ([] : tl) = withIndex (i + 1, 0) tl
        withIndex (i, j) ((c : s) : tl)
            | c == 'Q' = (i, j) : withIndex (i, j + 1) (s : tl)
            | otherwise = withIndex (i, j + 1) (s : tl)

listToBoard :: [(Int, Int)] -> [[Char]]
listToBoard board = L.map (\xs -> (L.map (\x -> if L.elem x board then 'Q' else '.') xs)) [[(i, j) | j <- [0..size-1]] | i <- [0..size-1]]

search :: [(Int, Int)] -> [[(Int, Int)]]
search board =
    if check board
    then
        if L.length board == size
        then [board]
        else L.intercalate [] $ L.map (\x -> search $ x : board) [(i, j) | i <- [0..size-1], j <- [0..size-1]]
    else []

check :: [(Int, Int)] -> Bool
check [] = True
check ((i, j) : xs) = L.all (\(y, x) -> i /= y && j /= x && abs (i - y) /= abs (j - x)) xs && check xs

showAnswer :: [[(Int, Int)]] -> [String]
showAnswer [] = ["impossible"]
showAnswer (board : _) = listToBoard board

main :: IO ()
main = do
    board <- readBoard
    let boards = search $ boardToList board
    M.mapM_ putStrLn $ showAnswer boards
