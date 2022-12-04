module AOC_2022_4 where
import Data.List.Split (wordsBy)
import System.IO

main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = count1 (lines s) 0

count1 :: [String] -> Int -> String
count1 [] c = show c
count1 (x : xs) c = let m = wordsBy (\t -> if ((t == '-') || (t == ',')) then True else False) x
                        x1 = read (m!!0) :: Int
                        x2 = read (m!!1) :: Int
                        x3 = read (m!!2) :: Int
                        x4 = read (m!!3) :: Int in
                                if ((((x3 <= x1) && (x2 <= x4)) || ((x3 >= x1) && (x2 >= x4)))) then count1 xs (c + 1) else count1 xs c
                                
main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents

analyze2 :: String -> String
analyze2 s = count2 (lines s) 0

count2 :: [String] -> Int -> String
count2 [] c = show c
count2 (x : xs) c = let m = wordsBy (\t -> if ((t == '-') || (t == ',')) then True else False) x
                        x1 = read (m!!0) :: Int
                        x2 = read (m!!1) :: Int
                        x3 = read (m!!2) :: Int
                        x4 = read (m!!3) :: Int in
                                if (((x3 <= x2) && (x3 >= x1))|| ((x1 <= x4) && (x1 >= x3))) then count2 xs (c + 1) else count2 xs c                                