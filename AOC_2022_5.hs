module AOC_2022_5 where
import Data.List.Split (splitOn)
import Data.Char (isNumber)
import System.IO
{-
https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
-}

main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 $ contents

lst = ["ZPMHR","PCJB","SNHGLCD","FTMDQSRL","FSPQBTZM","TFSZBG","NRV","PGLTDVCM","WQNJFML"]

analyze1 :: String -> String
analyze1 s = map (\x -> if ((length x) > 0) then x !! ((length x) - 1) else ' ') (count1 (splitOn "\n" s) lst)

replace :: Int -> String -> [String] -> [String]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

count1 :: [String] -> [String] -> [String]
count1 []       s = s
count1 (x : xs) s = let m = (filter (all isNumber)) $ words x
                        x1 = (read (m !! 0) :: Int)
                        x2 = (read (m !! 1) :: Int) - 1
                        x3 = (read (m !! 2) :: Int) - 1
                        x4 = take ((length (s !! x2)) - x1) (s !! x2)
                        x5 = drop ((length (s !! x2)) - x1) (s !! x2)
                        s' = replace x2 x4 s
                        s'' = replace x3 ((s !! x3) ++ (reverse x5)) s'
                                                                        in count1 xs s''
                                                                
main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents

analyze2 :: String -> String
analyze2 s = map (\x -> if ((length x) > 0) then x !! ((length x) - 1) else '0') (count2 (splitOn "\n" s) lst)

count2 :: [String] -> [String] -> [String]
count2 []       s = s
count2 (x : xs) s = let m = (filter (all isNumber)) $ words x
                        x1 = (read (m !! 0) :: Int)
                        x2 = (read (m !! 1) :: Int) - 1
                        x3 = (read (m !! 2) :: Int) - 1
                        x4 = take ((length (s !! x2)) - x1) (s !! x2)
                        x5 = drop ((length (s !! x2)) - x1) (s !! x2)
                        s' = replace x2 x4 s
                        s'' = replace x3 ((s !! x3) ++ x5) s'
                                                                in count2 xs s''                                                                