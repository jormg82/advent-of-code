
import qualified Data.List as L

type Replacement = (String, String)


readReplacement :: [String] -> Replacement
readReplacement [s1,_,s2] = (s1, s2)


scanInput :: String -> String -> [Replacement] -> [String]
scanInput ini [] reps = []
scanInput ini [c] reps =
  [ini ++ s2 | (s1, s2) <- reps, s1 == [c]]
scanInput ini (c:d:ds) reps =
  [ini ++ s2 ++ (d:ds) | (s1, s2) <- reps, s1 == [c]] ++
  [ini ++ s2 ++ ds | (s1, s2) <- reps, s1 == [c,d]] ++
  scanInput (ini ++ [c]) (d:ds) reps

transform :: String -> String -> Int
transform rep input =
  length $ L.nub $ scanInput "" input replacements
  where
    replacements = map readReplacement (map words $ lines rep)
        

main = do rep <- readFile "replacements.txt"
          input <- readFile "input.txt"
          print (transform rep input)

