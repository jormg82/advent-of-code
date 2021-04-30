
data SueInfo = SueInfo { number :: Int
                       , children :: Maybe Int
                       , cats :: Maybe Int
                       , samoyeds :: Maybe Int
                       , pomeranians :: Maybe Int
                       , akitas :: Maybe Int
                       , vizslas :: Maybe Int
                       , goldfish :: Maybe Int
                       , trees :: Maybe Int
                       , cars :: Maybe Int
                       , perfumes :: Maybe Int
                       } deriving (Show)

sueGif = SueInfo { number = 0
                 , children = Just 3
                 , cats = Just 7
                 , samoyeds = Just 2
                 , pomeranians = Just 3
                 , akitas = Just 0
                 , vizslas = Just 0
                 , goldfish = Just 5
                 , trees = Just 3
                 , cars = Just 2
                 , perfumes = Just 1
                 }

voidSue = SueInfo { number = 0
                  , children = Nothing
                  , samoyeds = Nothing
                  , pomeranians = Nothing
                  , akitas = Nothing
                  , vizslas = Nothing
                  , goldfish = Nothing
                  , trees = Nothing
                  , cars = Nothing
                  , perfumes = Nothing
                  , cats = Nothing
                  }
                   
groupFields :: [String] -> [[String]]
groupFields [] = []
groupFields [a, b] = [[a, b]]
groupFields (a:b:bs) = [a, b]:groupFields bs

readField :: [String] -> SueInfo -> SueInfo
readField = readComp . map removePunct

readComp :: [String] -> SueInfo -> SueInfo
readComp ["Sue", n] sue = sue {number = read n}
readComp ["children", n] sue = sue {children = Just (read n)}
readComp ["cats", n] sue = sue {cats = Just (read n)}
readComp ["samoyeds", n] sue = sue {samoyeds = Just (read n)}
readComp ["pomeranians", n] sue = sue {pomeranians = Just (read n)}
readComp ["akitas", n] sue = sue {akitas = Just (read n)}
readComp ["vizslas", n] sue = sue {vizslas = Just (read n)}
readComp ["goldfish", n] sue = sue {goldfish = Just (read n)}
readComp ["trees", n] sue = sue {trees = Just (read n)}
readComp ["cars", n] sue = sue {cars = Just (read n)}
readComp ["perfumes", n] sue = sue {perfumes = Just (read n)}

removePunct :: String -> String
removePunct = filter (not . flip elem ":,")
                
readLine :: String -> SueInfo
readLine = foldr readField voidSue . groupFields . words

matchField :: Ord a => (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
matchField op (Just x) (Just y) = op x y
matchField _ _ _ = True

match :: SueInfo -> SueInfo -> Bool
match s1 s2 = matchField (==) (children s1) (children s2) &&
              matchField (<) (cats s1) (cats s2) &&
              matchField (==) (samoyeds s1) (samoyeds s2) &&
              matchField (>) (pomeranians s1) (pomeranians s2) &&
              matchField (==) (akitas s1) (akitas s2) &&
              matchField (==) (vizslas s1) (vizslas s2) &&
              matchField (>) (goldfish s1) (goldfish s2) &&
              matchField (<) (trees s1) (trees s2) &&
              matchField (==) (cars s1) (cars s2) &&
              matchField (==) (perfumes s1) (perfumes s2)
             

transform :: String -> [Int]
transform = map number. filter (match sueGif) . map readLine . lines

main = readFile "input.txt" >>= print . transform

