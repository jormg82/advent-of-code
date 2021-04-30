
import qualified Data.List as L

step :: String -> String
step = concat . map (\xs -> show (length xs) ++ [head xs]) . L.group

las :: Int -> String -> String
las n = (!!n) . iterate step

main = print $ length $ las 50 "3113322113"

