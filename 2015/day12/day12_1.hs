
import qualified Data.List.Split as L

transform :: String -> Int
transform = sum . map read . L.wordsBy (`notElem` notsep)
            where notsep = '-':['0'..'9']


main = readFile "input.txt" >>= print . transform

