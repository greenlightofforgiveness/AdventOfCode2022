module AOC_2022_11 where
import Data.List (sort)
import qualified Data.Map as Map

{-
Idea:
https://www.reddit.com/r/adventofcode/comments/zifqmh/comment/j0avpoa/
-}

monkeys_func = [((\x -> x * 3), (\x -> if (x `mod` 11 == 0) then 7 else 2)), ((\x -> x + 3), (\x -> if (x `mod` 2 == 0) then 2 else 0)), ((\x -> x + 5), (\x -> if (x `mod` 5 == 0) then 7 else 5)), ((\x -> x * 19), (\x -> if (x `mod` 7 == 0) then 6 else 4)), ((\x -> x + 1), (\x -> if (x `mod` 17 == 0) then 6 else 1)), ((\x -> x + 2), (\x -> if (x `mod` 19 == 0) then 4 else 3)), ((\x -> x * x), (\x -> if (x `mod` 3 == 0) then 0 else 1)), ((\x -> x + 8), (\x -> if (x `mod` 13 == 0) then 3 else 5))]

monkeys_items = Map.fromList ([(0,[75,63]), (1,[65, 79, 98, 77, 56, 54, 83, 94]), (2,[66]), (3,[51, 89, 90]), (4,[75, 94, 66, 90, 77, 82, 61]), (5,[53, 76, 59, 92, 95]), (6,[81, 61, 75, 89, 70, 92]), (7, [81, 86, 62, 87])])

monkeys_lcm = foldr lcm 1 [11, 2, 5, 7, 17, 19, 3, 13]

{-
monkeys_func = [((\x -> x * 19), (\x -> if (x `mod` 23 == 0) then 2 else 3)), ((\x -> x + 6), (\x -> if (x `mod` 19 == 0) then 2 else 0)), ((\x -> x * x), (\x -> if (x `mod` 13 == 0) then 1 else 3)), ((\x -> x + 3), (\x -> if (x `mod` 17 == 0) then 0 else 1))]

monkeys_items = Map.fromList ([(0,[79,98]), (1,[54, 65, 75, 74]), (2,[79, 60, 97]), (3,[74])]) 

monkeys_lcm = foldr lcm 1 [23, 19, 13, 17]
-}

monkeys_insp = Map.fromList [(a, b) | a <- [0..7], b <- [0]]

analyze :: String
analyze = show (count1 monkeys_func monkeys_items monkeys_insp monkeys_lcm 0)

append :: [Int] -> [Int] -> [Int]
append x y = y ++ x

insert' :: [Int] -> [Int] -> Map.Map Int [Int] -> Map.Map Int [Int]
insert' [] y z = z 
insert' (x : xs) (y : ys) z = insert' xs ys (Map.insertWith (append) x [y] z)
               
count1 :: [(Int -> Int, Int -> Int)] -> Map.Map Int [Int] -> Map.Map Int Int -> Int -> Int -> Int
count1 m items insp mi 80000 = let x = reverse $ sort $ Map.elems insp in (x !! 0) * (x !! 1)
count1 m items insp mi i = let n = i `mod` 8
                               f1 = fst (m !! n)
                               f2 = snd (m !! n)
                               f3 = (\x -> x `mod` mi)
                               items' = items Map.! n
                               mov = map (f2.f1) items'
                               items'' = map (f3.f1) items'
                                in count1 m (insert' mov items'' (Map.insert n [] items)) (Map.insertWith (+) n (length items') insp) mi (i + 1)