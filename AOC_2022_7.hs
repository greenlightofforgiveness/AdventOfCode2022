module AOC_2022_7 where
import Data.List.Split (splitOn)
import Data.List (isPrefixOf,dropWhileEnd,sort)
import Data.Char (isNumber)
import qualified Data.Map as Map
import System.IO

main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ show $ analyze1 $ contents                                               

analyze1 :: String -> Int
analyze1 s = sum $ filter (< 100000) $  Map.elems (Map.fromListWith (+) (count1 (lines s) "" []))
-- analyze1 s = sum $ Map.fold (\x y -> if (x <= 100000) then [x] ++ y else y) [] (Map.fromListWith (+) (count1 (lines s) "" []))

main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ show $ analyze2 $ contents                                               

analyze2 :: String -> Int
analyze2 s = let x = Map.elems (Map.fromListWith (+) (count1 (lines s) "" []))
                 x' = (head (reverse (sort x)) - 40000000) in minimum $ filter (>= x') x

-- key for folder B in folder A (.../A/B/) = "AB"
f :: [String] -> [String]
f [x] = if (x == "") then [""] else []
f [x1, x2] = if (x1 == "") then [""] ++ [x1 ++ x2] else [x1 ++ x2]
f (x1 : x2 : xs) = if (x1 == "") then [""] ++ [x2] ++ f (x2 : xs) else [x1 ++ x2] ++ f (x2 : xs)

count1 :: [String] -> String -> [(String, Int)] -> [(String, Int)]
count1 [] currentDir paths = paths
count1 (x : xs) currentDir paths = if (x == "$ cd /") then count1 xs "/" paths else if (x == "$ ls") then count1 xs currentDir paths else if (x == "$ cd ..") then count1 xs (dropWhileEnd (\x -> if (x == '/') then False else True) (init currentDir)) paths else if (isPrefixOf "$ cd" x) then count1 xs (currentDir ++ (drop 5 x)  ++ "/" ) paths else if (isPrefixOf "dir" x) then count1 xs currentDir paths else count1 xs currentDir (paths ++ [(a, b) | a <- f (splitOn "/" (init currentDir)), b <- [(read (filter isNumber (takeWhile (/= ' ') x)) :: Int)]])