module AOC_2022_2 where
import Data.List.Split  (splitOn)
import System.IO

main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = show (count1 (splitOn "\n" s) 0)

count1 :: [String] -> Int -> Int
count1 [] x = x
count1 ("A X" : xs) x = count1 xs x + 1 + 3
count1 ("A Y" : xs) x = count1 xs x + 2 + 6
count1 ("A Z" : xs) x = count1 xs x + 3 + 0
count1 ("B X" : xs) x = count1 xs x + 1 + 0
count1 ("B Y" : xs) x = count1 xs x + 2 + 3
count1 ("B Z" : xs) x = count1 xs x + 3 + 6
count1 ("C X" : xs) x = count1 xs x + 1 + 6
count1 ("C Y" : xs) x = count1 xs x + 2 + 0
count1 ("C Z" : xs) x = count1 xs x + 3 + 3
count1 ( _ : xs)    x = count1 xs x + 0

main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents

analyze2 :: String -> String
analyze2 s = show (count2 (splitOn "\n" s) 0)

count2 :: [String] -> Int -> Int
count2 [] x = x
count2 ("A X" : xs) x = count2 xs x + 0 + 3
count2 ("A Y" : xs) x = count2 xs x + 3 + 1
count2 ("A Z" : xs) x = count2 xs x + 6 + 2
count2 ("B X" : xs) x = count2 xs x + 0 + 1
count2 ("B Y" : xs) x = count2 xs x + 3 + 2
count2 ("B Z" : xs) x = count2 xs x + 6 + 3
count2 ("C X" : xs) x = count2 xs x + 0 + 2
count2 ("C Y" : xs) x = count2 xs x + 3 + 3
count2 ("C Z" : xs) x = count2 xs x + 6 + 1
count2 ( _ : xs)    x = count2 xs x + 0