
type Grid = [[Int]]

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n-1) f (f x)

switch :: Int -> Int -> Int
switch 1 2 = 1 
switch 1 3 = 1 
switch 0 3 = 1 
switch _ _ = 0 

val :: Char -> Int
val '#' = 1
val '.' = 0

shiftN :: Grid -> Grid 
shiftN = (++[repeat 0]) . tail

shiftS :: Grid -> Grid 
shiftS = (repeat 0:) . init

shiftE :: Grid -> Grid 
shiftE = map ((++[0]) . tail)

shiftW :: Grid -> Grid 
shiftW = map ((0:) . init)

shiftNW :: Grid -> Grid 
shiftNW = (++[repeat 0]) . map (++[0]) . map tail . tail

shiftSW :: Grid -> Grid 
shiftSW = (repeat 0:) . map (++[0]) . map tail . init

shiftNE :: Grid -> Grid 
shiftNE = (++[repeat 0]) . map (0:) . map init . tail

shiftSE :: Grid -> Grid 
shiftSE = (repeat 0:) . map (0:) . map init . init

shiftList = [shiftN, shiftS, shiftE, shiftW,
             shiftNW, shiftSW, shiftNE, shiftSE]

correctCorners :: Grid -> Grid
correctCorners grid = newFirstRow:(init $ tail grid) ++ [newLastRow]
  where
    firstRow = head grid
    lastRow = last grid 
    newFirstRow = 1:(init $ tail firstRow) ++ [1]
    newLastRow = 1:(init $ tail lastRow) ++ [1]

calculateGrid :: Grid -> [[Int]] -> Grid
calculateGrid grid info =
  correctCorners $ zipWith (zipWith switch) grid info

onNeigh :: Grid -> [[Int]]
onNeigh grid = foldr1 (zipWith (zipWith (+))) $ map ($grid) shiftList

step :: Grid -> Grid
step grid = calculateGrid grid (onNeigh grid)

transform :: String -> Int
transform = sum . map (length . filter (==1)) .
            applyN 100 step . map (map val) . lines

main = readFile "input.txt" >>= print . transform

