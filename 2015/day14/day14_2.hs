

simulation :: Int -> (Int, Int, Int) -> Int
simulation time (v, t, r) = v*(cycles*t+min rem t)
  where (cycles, rem) = time `divMod` (t+r)


givePoints :: [Int] -> [Int]
givePoints xs = map (\x -> if x == m then 1 else 0) xs
                where m = maximum xs

transform :: Int -> String -> Int 
transform time input =
  maximum $ foldr1 (zipWith (+)) $ map givePoints partialResults
  where
    lineWords = map (drop 3 . words) $ lines input
    lineInfo (n:_:_:m:_:_:_:_:_:_:t:_) = (read n, read m, read t)
    infoRei = map lineInfo lineWords
    partialResults = map (\t -> map (simulation t) infoRei) [1..time]


main = readFile "input.txt" >>= print . transform 2503
