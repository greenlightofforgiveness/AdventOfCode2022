module AOC_2022_3 where
import Data.List (partition)
import Data.Char (ord)
import System.IO

main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = count1 (lines s) 0

count1 :: [String] -> Int -> String
count1 [] c = show c
count1 (x : xs) c = let l = length x `div` 2
                        s1 = take l x
                        s2 = drop l x
                        a = ord ((fst (partition (`elem` s1) s2))!!0) in if (a > 96) then count1 xs (c - 96 + a) else count1 xs (c - 64 + a + 26)
                        
main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents

analyze2 :: String -> String
analyze2 s = count2 (lines s) 0

count2 :: [String] -> Int -> String
count2 [] c = show c
count2 (x1 : x2 : x3 : xs) c = let   a = partition (`elem` x1) x2
                                     b = ord ((fst (partition (`elem` fst a) x3))!!0)
                                in
                                   if (b > 96) then count2 xs (c - 96 + b) else count2 xs (c - 64 + b + 26)