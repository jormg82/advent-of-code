
house' :: Int -> Int -> Int
house' n elve
  | elve > n `div` 2 = 0
  | otherwise = (if n `mod` elve == 0 then elve
                else 0) + house' n (elve+1)

house :: Int -> (Int, Int)
house n = (n, 11*(n + house' n (max (n `div` 50) 1)))

main = print $ house $ until ((>33099999) . snd . house) (+1) 780000

