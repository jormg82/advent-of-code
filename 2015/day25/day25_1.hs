
loop :: (Int, Int, Int) -> (Int, Int, Int)
loop (m, n, x)
  | n == 3083 && m == 2978 = (0, 0, x)
  | m == 1 = newx `seq` loop (n+1, 1, newx)
  | otherwise = newx `seq` loop (m-1, n+1, newx)
  where newx = x * 252533 `mod` 33554393

code = (\(_, _, x) -> x) $ loop (1, 1, 20151125)

main =  print code

