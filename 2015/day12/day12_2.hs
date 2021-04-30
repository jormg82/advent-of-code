
import qualified Data.List.Split as L

scanLevel :: Int -> String -> Int
scanLevel 0 ('{':cs) = scanLevel 1 cs
scanLevel 0 ('}':_) = 0
scanLevel 0 (':':'"':'r':'e':'d':'"':_) = 1
scanLevel 0 (c:cs) = scanLevel 0 cs
scanLevel n ('{':cs) = scanLevel (n+1) cs
scanLevel n ('}':cs) = scanLevel (n-1) cs
scanLevel n (c:cs) = scanLevel n cs


filterRed :: Int -> String -> String
filterRed _ [] = []
filterRed 0 ('{':cs) = filterRed (scanLevel 0 cs) cs
filterRed 0 ('}':cs) = filterRed 0 cs
filterRed 0 (c:cs) = c:filterRed 0 cs
filterRed n ('{':cs) = filterRed (n+1) cs
filterRed n ('}':cs) = filterRed (n-1) cs
filterRed n (c:cs) = filterRed n cs

transform :: String -> Int
transform = sum . map read .
            L.wordsBy (`notElem` notsep) . filterRed 0
            where notsep = '-':['0'..'9']


main = readFile "input.txt" >>= print . transform
