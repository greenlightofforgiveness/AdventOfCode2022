module AOC_2021_1 where
import Data.List
import System.IO     
    
main = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze contents

{-
https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html#v:words
http://learnyouahaskell.com/input-and-output
-}

analyze :: String -> String
analyze s = show (count (words s) (5000,0))

count :: [String] -> (Int,Int) -> Int
count [] (a, b) = b
count (x : xs) (a, b) = let x' = (read x :: Int) in if (x' > a) then count xs (x', b + 1) else count xs (x', b)