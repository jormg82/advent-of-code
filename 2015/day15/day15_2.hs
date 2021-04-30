

type IngredsInfo = [Int]

-- n in m parts
partitions :: Int -> Int -> [[Int]]
partitions 0 m = [replicate m 0]
partitions n 1 = [[n]]
partitions n m =
  concat $ map (\x -> map (x:) $ partitions (n-x) (m-1)) [0..n]


score :: [IngredsInfo] -> [Int] -> Int
score ifns part =
  foldr1 (*) $
  map (\x -> max 0 x) $
  changeCal $
  foldr1 (zipWith (+)) $
  zipWith (\x ifn -> map (x*) ifn) part ifns
  where changeCal [x, y, z, u, 500] = [x,y,z,u]
        changeCal [x, y, z, u, _] = [0, 0, 0, 0]


transform :: String -> Int 
transform input =
  maximum $ map (score ingredsInfo) (partitions 100 ningred)
  where
    lineWords = map (drop 2 . words) $ lines input
    lineInfo (n:_:m:_:o:_:p:_:q:_) =
      [read $ init n, read $ init m, read $ init o,
       read $ init p, read q]
    ingredsInfo = map lineInfo lineWords
    ningred = length ingredsInfo

main = readFile "input.txt" >>= print . transform

