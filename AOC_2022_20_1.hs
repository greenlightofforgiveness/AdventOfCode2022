module AOC_2022_20_1 where
import Data.List.Split (splitOn)
import System.IO

main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ show $ analyze1 $ contents
    -- writeFile "output.txt" $ show $ analyze1 $ contents
    
replace :: Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list    

analyze1 :: String -> Int
analyze1 s = let lst = (map (\x -> read x :: Int) (lines s))
                 n = length lst
                 lst' = zipWith (\a b -> (a, b)) lst [0 .. (n - 1)]
                        in count1 lst' n 0

find1 :: [(Int, Int)] -> Int
find1 ((p1, p2) : xs) = if (p1 == 0) then p2 else find1 xs
                    
find2 :: Int -> Int -> Int -> [(Int, Int)] -> Int -> Int
find2 _ _ _ [] s = s
find2 a b c ((p1, p2) : xs) s = if ((p2 == a) || (p2 == b) || (p2 == c)) then find2 a b c xs (s + p1) else find2 a b c xs s
                        
                        
count1 :: [(Int,Int)] -> Int -> Int -> Int
count1 lst n 5000 = let x = find1 lst
                        x1 = (x + 1000) `mod` 5000
                        x2 = (x + 2000) `mod` 5000
                        x3 = (x + 3000) `mod` 5000
                                in find2 x1 x2 x3 lst 0
count1 lst n i = let (a', b') = lst !! i
                     x = if (a' >= 0) then let a'' = a' `mod` (n - 1) in if (a'' + b') >= (n - 1) then a'' - n + 1 + b' else a'' + b' else let a'' = (abs a') `mod` (n - 1) in if (a'' >= b') then n - 1 - (a'' - b') else b' - a''
                     lst' = map (\(a, b) -> if ((b >= b') && (b <= x)) then (a, b - 1) else if ((b >= x) && (b <= b')) then (a, b + 1) else (a, b)) lst
                     lst'' = replace i (a', x) lst'
                        in count1 lst'' n (i + 1)