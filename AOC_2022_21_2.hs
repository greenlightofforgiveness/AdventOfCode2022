module AOC_2022_21_2 where
import Data.List.Split (splitOn, wordsBy)
import Data.List (all)
import Data.Char (isDigit)
import qualified Data.Map as M
import System.IO

main = do  
    contents <- readFile "input.txt"  
    putStrLn $ show $ analyze $ contents

analyze :: String -> Double
analyze s = solver (equation (toWaitingList (lines s) []) M.empty) 0

toWaitingList :: [String] -> [(String, [String])] -> [(String, [String])]
toWaitingList [] m = m
toWaitingList (x : xs) m =  let x' = wordsBy (\x -> (x == ' ') || (x == ':')) x
                                t = if (head x' == "root") then map (\c -> if ((c == "+") || (c == "*") || (c == "/")) then "-" else c) (tail x') else (tail x')
                                        in toWaitingList xs ([(head x', t)] ++ m)

equation :: [(String, [String])] -> M.Map String String -> String
equation [] a = a M.! "root"
equation w a  = let p = motion w ([], a) in equation (fst p) (snd p)

motion :: [(String, [String])] -> ([(String, [String])], M.Map String String) -> ([(String, [String])], M.Map String String)
motion [] (w', a') = (w', a')
motion ((k, v): ws) (w', a') = if (k == "humn") then motion ws (w', M.insert k "(X)" a') else if (length v == 1) then motion ws (w', M.insert k (v !! 0) a') else if ((M.member (v !! 0) a') && (M.member (v !! 2) a')) then motion ws (w', M.insert k (f' (a' M.! (v !! 0)) (v !! 1) (a' M.! (v !! 2))) a') else motion ws ([(k, v)] ++ w', a')

f :: String -> Double -> Double -> Double
f x = if (x == "+") then (\a b -> a + b) else if (x == "-") then (\a b -> a - b) else if (x == "*") then (\a b -> a * b) else (\a b -> a / b)

f' :: String -> String -> String -> String
f' x y z = if ((all (/='X') x) && (all (/='X') z)) then show ((f y) (read x :: Double) (read z :: Double)) else ("(" ++ x ++ " " ++ y ++ " " ++ z ++ ")")

solver :: String -> Double -> Double
solver str acc = if (str == "(X)") then acc
                        else let str' = tail (init str)
                                 left_or_right = let x = head str' in if ((isDigit x) || (x == '-')) then "l" else "r"
                                 eq_parts = if (left_or_right == "l") then words (takeWhile ( /= '(') str') else words (reverse (takeWhile ( /= ')') (reverse str')))
                                 eq_number = if (left_or_right == "l") then read (eq_parts !! 0) :: Double else read (eq_parts !! 1) :: Double
                                 eq_op = if (left_or_right == "l") then eq_parts !! 1 else eq_parts !! 0
                                 str'' = if (left_or_right == "l") then (dropWhile ( /= '(') str') else (reverse (dropWhile ( /= ')') (reverse str')))
                                 acc' = if (eq_op == "+") then acc - eq_number else if (eq_op == "*") then acc / eq_number else if ((eq_op == "-") && (left_or_right == "l")) then eq_number - acc  else if ((eq_op == "-") && (left_or_right == "r")) then eq_number + acc  else if ((eq_op == "/") && (left_or_right == "l")) then eq_number / acc  else if ((eq_op == "/") && (left_or_right == "r")) then eq_number * acc else 0
                                       in solver str'' acc'