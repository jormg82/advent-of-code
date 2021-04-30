

simulation :: Int -> (Int, Int, Int) -> Int
simulation time (v, t, r) = v*(cycles*t+min rem t)
  where (cycles, rem) = time `divMod` (t+r)

transform :: Int -> String -> String 
transform time input =
  show $ maximum $ map (simulation time) infoRei
  where
    lineWords = map (drop 3 . words) $ lines input
    lineInfo (n:_:_:m:_:_:_:_:_:_:t:_) = (read n, read m, read t)
    infoRei = map lineInfo lineWords


main = readFile "input.txt" >>= putStrLn . transform 2503

