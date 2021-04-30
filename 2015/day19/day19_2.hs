
-- Esta solucion consiste en ir sustituyendo al azar partes derechas de producciones
-- q se encuentren en el input por partes izquierdas
-- He eliminado todo lo que tenia H de la gramatica pq las H no aparecen en el input
-- Habia otra cadena que daba problemas, la reduje en el input antes de procesarlo

import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Map as M

type Symbol = String

type Replacements = M.Map Symbol [[Symbol]]


readReplacement :: [String] -> Replacements -> Replacements
readReplacement [s1,_,s2] = M.insertWith (++) s1 [getSymbs s2]


getSymbs :: String -> [Symbol]
getSymbs [] = []
getSymbs (x:xs) =
  let
    symb = x:takeWhile isLower xs
  in
    symb:getSymbs (dropWhile isLower xs)


subList :: Eq a => [a] -> [a] -> Maybe Int
subList _ [] = Nothing
subList as xxs@(x:xs)
  | as == take (length as) xxs = Just 0
  | otherwise = case subList as xs of
                  Just i -> Just (1+i)
                  Nothing -> Nothing


revReplace :: [(Symbol, [[Symbol]])] -> [Symbol] -> [Symbol]
--revReplace [] input = error $ concat"No se encontraron substituciones"
revReplace [] input = error $ concat input
revReplace ((k, ss):rs) input =
  case takeFirstJust $ map (\seq -> (seq, subList seq input)) ss of
    (seq, Just i)  -> let (pref, suf) = splitAt i input
                      in pref ++ [k] ++ drop (length seq) suf
    (_, Nothing) -> revReplace rs input
  where
    takeFirstJust [] = (undefined, Nothing)
    takeFirstJust ((p@(_, Just _)):xs) = p
    takeFirstJust ((p@(_, _)):xs) = takeFirstJust xs



step :: (Int, Replacements, [Symbol]) -> (Int, Replacements, [Symbol])
step (n, r, input) = (n+1,r,revReplace as input)
                     where as = M.assocs r


endCondition :: (Int, Replacements, [Symbol]) -> Bool
endCondition (_,_,ss) = ss == ["e"]


parse :: Replacements -> [Symbol] -> Int
parse rs input =
  (\(x,_,_) -> x) $ until endCondition step initial
  where initial = (0, rs, input)


transform :: String -> String -> String
transform rep input =
  show $ parse replacs (getSymbs input)
  --error $ show $ M.assocs replacs
  where
    rawReplacements = map words $ lines rep
    replacs = foldr readReplacement M.empty rawReplacements


main = do rep <- readFile "replacements.txt"
          input <- readFile "input.txt"
          print $ transform rep (concat $ lines input)
