

house' :: Int -> Int -> Int
house' n elve
  | elve > n `div` 2 = 0
  | otherwise = (if n `mod` elve == 0 then elve
                else 0) + house' n (elve+1)

house :: Int -> (Int, Int)
house n = (n, 10*(n + house' n 1))

main = print $ house $ until ((>=33100000) . snd . house) (+1) 700000

