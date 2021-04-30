
import qualified Data.List as L
import Data.Maybe

type Person = String


happiness :: [((Person, Person), Int)] -> [Person] -> Int
happiness info arrange =
  sum $ zipWith points arrange (tail arrange ++ [head arrange])
  where
    points p1 p2 = fromJust (lookup (p1, p2) info) +
                   fromJust (lookup (p2, p1) info)
                   

transform :: String -> String 
transform input = 
  show $ maximum $ map (happiness infoMap) perms 
  where
    lineWords = map words $ lines input
    lineInfo [a, _, "lose", n, _, _, _, _, _, _, b] = ((a, init b), -read n)
    lineInfo [a, _, "gain", n, _, _, _, _, _, _, b] = ((a, init b), read n)
    persons = L.nub $ map head lineWords
    infoMap = map lineInfo lineWords
    perms = map (head persons:) $ L.permutations (tail persons)


main = readFile "input.txt" >>= putStrLn . transform

