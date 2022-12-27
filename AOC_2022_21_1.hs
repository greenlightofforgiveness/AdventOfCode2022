module AOC_2022_21_1 where
import Data.List.Split (splitOn, wordsBy)
import qualified Data.Map as M
import System.IO

main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ show $ analyze1 $ contents

analyze1 :: String -> Double
analyze1 s = count1 (toWaitingList (lines s) []) M.empty

toWaitingList :: [String] -> [(String, [String])] -> [(String, [String])]
toWaitingList [] m = m
toWaitingList (x : xs) m =  let x' = wordsBy (\x -> (x == ' ') || (x == ':')) x
                                in toWaitingList xs ([(head x', tail x')] ++ m) 

count1 :: [(String, [String])] -> M.Map String Double -> Double
count1 [] a = a M.! "root"
count1 w a  = let p = motion w ([], a) in count1 (fst p) (snd p)

motion :: [(String, [String])] -> ([(String, [String])], M.Map String Double) -> ([(String, [String])], M.Map String Double)
motion [] (w', a') = (w', a')
motion ((k, v): ws) (w', a') = if (length v == 1) then motion ws (w', M.insert k (read (v !! 0) :: Double) a') else if ((M.member (v !! 0) a') && (M.member (v !! 2) a')) then motion ws (w', M.insert k ((f (v !! 1)) (a' M.! (v !! 0)) (a' M.! (v !! 2))) a') else motion ws ([(k, v)] ++ w', a')

f x = if (x == "+") then (\a b -> a + b) else if (x == "-") then (\a b -> a - b) else if (x == "*") then (\a b -> a * b) else (\a b -> a / b)