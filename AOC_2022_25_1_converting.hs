module AOC_2022_25_1 where
import Data.List.Split (splitOn)
import System.IO

main = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze $ contents

analyze :: String -> String
analyze s = fromIntToSnafu (count (lines s) 0) ""

count :: [String] -> Int -> Int
count [] a = a
count (x : xs) a  = count xs (a + (fromSnafuToInt x))

fromSnafuToInt :: String -> Int 
fromSnafuToInt x = fromSnafuToInt' x ((length x) - 1) 0

fromSnafuToInt' :: String -> Int -> Int -> Int
fromSnafuToInt' [] _ acc = acc
fromSnafuToInt' (x : xs) i acc = case x of '0' -> fromSnafuToInt' xs (i - 1) acc
                                           '1' -> fromSnafuToInt' xs (i - 1) (acc + 5^i)
                                           '2' -> fromSnafuToInt' xs (i - 1) (acc + 2 * 5^i)
                                           '-' -> fromSnafuToInt' xs (i - 1) (acc - 5^i)
                                           '=' -> fromSnafuToInt' xs (i - 1) (acc - 2 * 5^i)
                                           
fromIntToSnafu :: Int -> String -> String
fromIntToSnafu x acc = let a = x `mod` 5
                           b = x `div` 5 in
                               if (b < 5) then case (b, a) of (0,0) -> acc
                                                              (0,1) -> "1" ++ acc
                                                              (0,2) -> "2" ++ acc
                                                              (0,3) -> "1=" ++ acc
                                                              (0,4) -> "1-" ++ acc
                                                              (1,0) -> "10" ++ acc
                                                              (1,1) -> "11" ++ acc
                                                              (1,2) -> "12" ++ acc
                                                              (1,3) -> "2=" ++ acc
                                                              (1,4) -> "2-" ++ acc
                                                              (2,0) -> "20" ++ acc
                                                              (2,1) -> "21" ++ acc
                                                              (2,2) -> "22" ++ acc
                                                              (2,3) -> "1==" ++ acc
                                                              (2,4) -> "1=-" ++ acc
                                                              (3,0) -> "1=0" ++ acc
                                                              (3,1) -> "1=1" ++ acc
                                                              (3,2) -> "1=2" ++ acc
                                                              (3,3) -> "1-=" ++ acc
                                                              (3,4) -> "1--" ++ acc
                                                              (4,0) -> "1-0" ++ acc
                                                              (4,1) -> "1-1" ++ acc
                                                              (4,2) -> "1-2" ++ acc
                                                              (4,3) -> "10=" ++ acc
                                                              (4,4) -> "10-" ++ acc
                                           else case a of  0 -> fromIntToSnafu b ("0" ++ acc)
                                                           1 -> fromIntToSnafu b ("1" ++ acc)
                                                           2 -> fromIntToSnafu b ("2" ++ acc)
                                                           3 -> fromIntToSnafu (b + 1) ("=" ++ acc)
                                                           4 -> fromIntToSnafu (b + 1) ("-" ++ acc)