import Data.Array

dist :: String -> String -> Int
dist a [] = length a
dist [] b = length b
dist (a:as) (b:bs) 
 = min l (min m r)
 where
    l = dist as (b:bs) + 1
    r = dist (a:as) bs + 1
    m = dist as bs + fromEnum (a /= b)

dist' :: String -> String -> Int
dist' a b = table !! length a !! length b
   where
      table = [[dist1 (take i a) (take j b) | j <- [0..length b]] | i <- [0..length a]]
      dist1 :: String -> String -> Int
      dist1 a [] = length a
      dist1 [] b = length b
      dist1 x y = min l (min m r)
         where
            l = (table !! (length x - 1) !! length y) + 1
            r = (table !! length x !! (length y - 1)) + 1
            m = (table !! (length x - 1) !! (length y - 1)) + fromEnum (x /= y)


tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate (u, v) f = array (u, v) [(i, f i) | i <- range (u, v)]

fromList :: [a] -> Array Int a
fromList xs = listArray (0, length xs - 1) xs

dist'' :: String -> String -> Int
dist'' a b = table ! (n, m)
   where
      table = tabulate ((0, 0), (n, m)) memo
      (n, m) = (length a, length b)
      a' = fromList a
      b' = fromList b
      memo :: (Int, Int) -> Int
      memo (x, 0) = x
      memo (0, y) = y
      memo (x, y) = minimum [table ! (x - 1, y) + 1,
                             table ! (x, y - 1) + 1,
                             table ! (x - 1, y - 1) + if a' ! (x - 1) == b' ! (y - 1) then 0 else 1]



