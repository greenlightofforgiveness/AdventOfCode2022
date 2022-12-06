module AOC_2022_6 where
import System.IO

main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = show (count1 s 0)

count1 :: String -> Int -> Int
count1 [] c = c + 4
count1 (x1 : x2 : x3 : x4 : xs) c = if ((x1 /= x2) && (x1 /= x3) && (x1 /= x4) && (x2 /= x3) && (x2 /= x4) && (x4 /= x3)) then count1 [] c else count1 (x2 : x3 : x4 : xs) (c + 1)
count1 _ c = c

main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents

analyze2 :: String -> String
analyze2 s = show (count2 s 0)

{-
https://stackoverflow.com/questions/31036474/haskell-checking-if-all-list-elements-are-unique
-}
allDifferent :: (Eq a) => [a] -> Bool
allDifferent list = case list of
    []      -> True
    (x:xs)  -> if x `elem` xs then False else allDifferent xs

count2 :: String -> Int -> Int
count2 [] c = c + 14
count2 (x1 : x2 : x3 : x4 : x5 : x6  : x7 : x8 : x9 : x10 : x11 : x12 : x13 : x14 : xs) c = if (allDifferent [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14]) then count2 [] c else count2 (x2 : x3 : x4 : x5 : x6  : x7 : x8 : x9 : x10 : x11 : x12 : x13 : x14 : xs) (c + 1)
count2 _ c = c                             
                                