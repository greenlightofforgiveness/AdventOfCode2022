module AOC_2022_1 where
import Data.List.Split (splitOn)
import System.IO
   
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents
    
main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents    

analyze1 :: String -> String
analyze1 s = show (count1 (splitOn "\n" s) (0, 0))

count1 :: [String] -> (Int, Int) -> Int
count1 [] (s, a)        = a
count1 ("" : xs) (s, a) = if (a > s) then count1 xs (0, a) else count1 xs (0, s)
count1 (x : xs) (s, a)  = let x' = (read x :: Int) in count1 xs (s + x', a)

analyze2 :: String -> String
analyze2 s = show (count2 (splitOn "\n" s) (0, 0, 0, 0))

count2 :: [String] -> (Int, Int, Int, Int) -> Int
count2 [] (s, a, b, c)        = a + b + c
count2 ("" : xs) (s, a, b, c) = if (s > a) then count2 xs (0, s, a, b) 
                                                else if (s > b) then count2 xs (0, a, s, b)
                                                else if (s > c) then count2 xs (0, a, b, s)
                                                else count2 xs (0, a, b, c)
count2 (x : xs) (s, a, b, c)  = let x' = (read x :: Int) in count2 xs (s + x', a, b, c)