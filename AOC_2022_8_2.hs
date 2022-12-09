module AOC_2022_8_2 where
import Data.List (findIndex)
import System.IO

main = do  
    contents <- readFile "input.txt"  
    putStrLn $ show (analyze1 contents)

analyze1 :: String -> Int
analyze1 s = let m = lines s
                 l = length m
                 m' = take (l - 2) (drop 1 m) in (count1 m' m 0)

               
count1 :: [String] -> [String] -> Int -> Int
count1 [] forest c = c
count1 (x : xs) forest c  = let         size = length x
                                        size' = length forest
                                        x' = map (\a' -> read a' :: Int) (map (:[]) x)
                                        x'' a = [(forest !! i) !! a | i <- [0..size' - 1]]
                                        cond lst a = let x1 = findIndex (>= lst !! a) (reverse (take a lst))
                                                         x2 = findIndex (>= lst !! a) (drop (a + 1) lst)
                                                         l = length lst - (a + 1)
                                                                in if ((x1 == Nothing) && (x2 == Nothing)) then l*a else if (x1 == Nothing) then a * ((\(Just x) -> (x + 1)) x2) else if (x2 == Nothing) then l * ((\(Just x) -> (x + 1)) x1) else (\(Just a) (Just b) -> (a + 1) * (b + 1)) x1 x2
                                        ans = maximum [(cond x' a) * (cond (x'' a) (size' - length xs - 2)) | a <- [1..size - 2]]
                                             in if (ans > c) then count1 xs forest ans else count1 xs forest c