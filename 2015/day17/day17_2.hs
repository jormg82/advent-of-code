

powerset :: [Int] -> [[Int]]
powerset [] = [[]]
powerset (x:xs) = map (x:) pxs ++ pxs
                  where pxs = powerset xs

fit :: [Int] -> Bool
fit = (== 150) . sum

transform :: String -> Int
transform = minimum . map length . filter fit . powerset . map read . lines

main = readFile "input.txt" >>= print . transform

