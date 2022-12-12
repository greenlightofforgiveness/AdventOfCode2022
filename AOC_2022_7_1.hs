module AOC_2022_7 where
import Data.List.Split (splitOn)
import Data.List (isPrefixOf,dropWhileEnd)
import Data.Char (isNumber)
import qualified Data.Map as Map
import System.IO

main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ show $ analyze1 $ contents                                               

analyze1 :: String -> Int
analyze1 s = sum $ filter (< 100000) $ Map.elems (Map.fromListWith (+) (count1 (lines s) "" []))
-- analyze1 s = sum $ Map.fold (\x y -> if (x <= 100000) then [x] ++ y else y) [] (Map.fromListWith (+) (count1 (lines s) "" []))

-- key for folder B in folder A (.../A/B/) = "AB"
f [] = []
f [x] = if (x == "") then [""] else []
f (x1 : x2 : xs) = [x1 ++ x2] ++ f (x2 : xs)

count1 :: [String] -> String -> [(String, Int)] -> [(String, Int)]
count1 [] currentDir pathes = pathes
count1 (x : xs) currentDir pathes = if (x == "$ cd /") then count1 xs "/" pathes else if (x == "$ ls") then count1 xs currentDir pathes else if (x == "$ cd ..") then count1 xs (dropWhileEnd (\x -> if (x == '/') then False else True) (init currentDir)) pathes else if (isPrefixOf "$ cd" x) then count1 xs (currentDir ++ (drop 5 x)  ++ "/" ) pathes else if (isPrefixOf "dir" x) then count1 xs currentDir pathes else count1 xs currentDir (pathes ++ [(a, b) | a <- f (splitOn "/" (init currentDir)), b <- [(read (filter isNumber (takeWhile (/= ' ') x)) :: Int)]])