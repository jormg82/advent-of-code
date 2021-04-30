
import qualified Data.List as L
import Data.Maybe

type City = String


pathDist :: [((City, City), Int)] -> [City] -> Int
pathDist dists cities =
  sum $ zipWith distance cities (tail cities)
  where
    distance c1 c2 = fromJust $ lookup (c1, c2) dists
    

transform :: String -> String 
transform str = 
  show $ maximum $ map (pathDist distMap) perms 
  where
    lineWords = map words $ lines str
    cities = L.nub $ concat (map lineCities lineWords)
    distMap = concat $ map lineDist lineWords
    lineDist [a, _, b, _, d] = [((a, b), read d), ((b, a), read d)] 
    lineCities [a, _, b, _, _] = [a, b]
    perms = L.permutations cities


main = readFile "input.txt" >>= putStrLn . transform

