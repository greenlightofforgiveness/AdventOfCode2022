module AOC_2022_25_1 where
import qualified Data.Map as Map
import System.IO

{-
Idea: add all SNAFU numbers directly, without converting them to decimal numeral system.

Equation:
b — carry in bit
s — sum of bits (from all numbers) at current position (current power of 5)
i — current position (power)
y — carry out bit
x — result bit at current position; x can be equal to {0,1,2,-1,-2}

(b + s) * 5^i = x * 5^i + y * 5^(i + 1)
(b + s) = x + 5 * y

y = (b + s - x) div 5, if (b + s - x) mod 5 = 0

x = ?
(b + s) mod 5 -> result bit x:
0 -> 0
1 -> 1
2 -> 2
3 -> -2 (=)
4 -> -1 (-)

Carry out bit: y = (b + s - x) div 5
-}

main = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze $ contents

analyze :: String -> String
analyze s = let m = Map.fromListWith (++) $ concatMap (\x -> zipWith (\a b -> (a, [b])) [(length x - 1),(length x - 2)..0] x) (lines s)
                in sumAllNumbers m 0 0 ""

sumBits :: String -> Int -> Int
sumBits [] acc = acc
sumBits (x : xs) acc  = case x of    '0' -> sumBits xs acc
                                     '1' -> sumBits xs (acc + 1)
                                     '2' -> sumBits xs (acc + 2)
                                     '-' -> sumBits xs (acc - 1)
                                     '=' -> sumBits xs (acc - 2)

sumAllNumbers :: Map.Map Int [Char] -> Int -> Int -> String -> String                               
sumAllNumbers m i b acc = if ((Map.notMember i m) && (b == 0)) then acc else if (Map.notMember i m) then sumAllNumbers m (i + 1) (snd (f 0 b)) (fst (f 0 b) ++ acc) else let s = sumBits (m Map.! i) 0 in sumAllNumbers m (i + 1) (snd (f s b)) (fst (f s b) ++ acc)

f :: Int -> Int -> (String, Int)
f s b = case (b + s) `mod` 5 of    0 -> ("0", (b + s) `div` 5)
                                   1 -> ("1", (b + s - 1) `div` 5)
                                   2 -> ("2", (b + s - 2) `div` 5)
                                   3 -> ("=", (b + s + 2) `div` 5)
                                   4 -> ("-", (b + s + 1) `div` 5)