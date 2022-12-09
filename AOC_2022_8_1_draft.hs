module AOC_2022_8 where
import System.IO

main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ show (analyze1 contents)

analyze1 :: String -> Int
analyze1 s = let m = lines s
                 l = length m
                 m' = take (l - 2) (drop 1 m) in (count1 m' m 0) + 4 * (length (m!!0)) - 4

               
count1 :: [String] -> [String] -> Int -> Int
count1 [] forest c = c
count1 (x : xs) forest c  = let         size = length x
                                        size' = length forest
                                        x' = map (\a' -> read a' :: Int) (map (:[]) x)
                                        x'' a = [(forest !! i) !! a | i <- [0..size' - 1]]
                                        cond lst a = ((lst !! a) > (maximum (take a lst))) || ((lst !! a) > (maximum (drop (a + 1) lst)))
                                        ans = length $ filter (== True) [cond x' a || cond (x'' a) (size' - length xs - 2) | a <- [1..size - 2]]
                                             in count1 xs forest (c + ans)