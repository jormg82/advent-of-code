

transform :: String -> String 
transform = show . sum . map (\c -> if c=='(' then 1 else -1) . concat . lines

main = readFile "input.txt" >>= putStrLn . transform

