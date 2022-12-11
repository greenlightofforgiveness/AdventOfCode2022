module AOC_2022_10 where
import System.IO

main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ show (analyze1 contents)

analyze1 :: String -> Int
analyze1 s = count1 (lines s) 1 0 0
               
count1 :: [String] -> Int -> Int -> Int -> Int
count1 [] reg cycle c = c
count1 (x : xs) reg cycle c = let d = take 4 x
                                  s = if (d == "addx") then (read (drop 5 x)) :: Int else 0
                                          in case d of "addx" -> let c' = if ((cycle + 1 - 20) `mod` 40 == 0) then (cycle + 1) * reg else if                      ((cycle + 2 - 20) `mod` 40 == 0) then (cycle + 2) * reg else 0
                                                                        in count1 xs (reg + s) (cycle + 2) (c + c')
                                                       "noop" -> let c' = if ((cycle + 1 - 20) `mod` 40 == 0) then (cycle + 1) * reg else 0
                                                                        in count1 xs reg (cycle + 1) (c + c')
                                                                  
main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ lines40 (analyze2 contents)

lines40 [] = []    
lines40 x = (take 40 x) ++ "\n" ++ lines40 (drop 40 x)
    
analyze2 :: String -> String
analyze2 s = count2 (lines s) 1 0 ""
               
count2 :: [String] -> Int -> Int -> String -> String
count2 [] reg cycle crt = crt
count2 (x : xs) reg cycle crt = let d = take 4 x
                                    s = if (d == "addx") then (read (drop 5 x)) :: Int else 0
                                        in case d of "addx" -> let t1 = if (((cycle + 1) `mod` 40 >= reg - 1) && ((cycle + 1) `mod` 40 <= reg                           + 1)) then "#" else "."
                                                                   t2 = if (((cycle + 2)`mod` 40 >= reg + s - 1) && ((cycle + 2)`mod` 40 <= reg + s + 1)) then "#" else "."
                                                                        in count2 xs (reg + s) (cycle + 2) (crt ++ t1 ++ t2)
                                                     "noop" -> let t1 = if (((cycle + 1) `mod` 40 >= reg - 1) && ((cycle + 1) `mod` 40 <= reg                   + 1)) then "#" else "."
                                                                        in count2 xs reg (cycle + 1) (crt ++ t1)                                                          