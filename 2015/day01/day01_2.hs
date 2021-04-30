
import Data.List


transform :: String -> String 
transform = show . fmap (+1) . findIndex (<0) . scanl1 (+) .
            map (\c -> if c=='(' then 1 else -1) . concat . lines

main = readFile "input.txt" >>= putStrLn . transform

