
import qualified Data.List as L
import Data.Char

has3Seq :: String -> Bool
has3Seq [] = False
has3Seq [x] = False
has3Seq [x, y] = False
has3Seq (x:y:z:zs) = (ord z-ord y==1 && ord y-ord x==1) || has3Seq (y:z:zs)

pairs :: String -> Bool
pairs = (>=2) . length . filter (/=1) . map length . L.group

valid :: String -> Bool
valid str = has3Seq str &&
            all (\c -> notElem c "iol") str &&
            pairs str

stepIncrement :: Char -> (Int, String) -> (Int, String)
stepIncrement c (carry, str)
  | carry == 0 = (0, c:str)
  | otherwise  = if c < 'z' then (0, chr (ord c +1):str)
                 else (1, 'a':str)

increment1 :: String -> String
increment1 = snd . foldr stepIncrement (1, [])

newPass :: String -> String
newPass = until valid increment1

main = print $ newPass "hepxcrrq"

