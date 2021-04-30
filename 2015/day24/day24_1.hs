
import Data.List ((\\), nub)

comb :: [Int] -> Int -> [[Int]]
comb [] 0 = [[]]
comb [] n = []
comb (x:xs) 0 = [[]]
comb (x:xs) n = map (x:) (comb xs (n-1)) ++ comb xs n


-- ojo, he puesto 5
--partitions ns =
--  [p1 | n  <- [6..l-2],
--        p1 <- comb ns n,
--        sum p1 == s,
--        any ((==s) . sum) $ concat $ map (comb (ns\\p1)) [1..(l-n-1)]]
--  where s = sum ns `div` 3
--        l = length ns

discard :: (a -> a -> Bool) -> [a] -> [a]
discard _ [] = []
discard _ [a] = [a]
discard f (a:b:bs) = if f a b then a:discard f (b:bs) else [a]


partitions :: [Int] -> [[Int]]
partitions ns =
  discard (\xs ys -> length xs == length ys) $
  filter (\p -> any ((==s) . sum) $ concat $ map (comb (ns\\p)) [1..(l-length p-1)]) $
  filter ((==s) . sum) $
  concat $
  map (comb ns) [1..l-2]
  where s = sum ns `div` 3
        l = length ns

calcParts :: String -> Int
calcParts = minimum . map (foldr1 (*)) . partitions . map read . lines


input :: [Int]
input = [1,3,5,11,13,17,19,23,29,31,37,41,43,47,53,59,67,
         71,73,79,83,89,97,101,103,107,109,113] 

-- for my input:
-- minimum . map (foldr1 (*)) . filter ((==508) . sum) $ comb input 6
-- minimum . map (foldr1 (*)) . filter ((==381) . sum) $ comb input 5

main = readFile "input.txt" >>= print . calcParts
