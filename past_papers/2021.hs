import Data.Array

-- Helpers
data Tree a = Leaf a | Node (Tree a) (Tree a)

fold :: (a -> a -> a) -> [a] -> a
fold f [x] = x
fold f (x : y : xs) = fold f (f x y : xs)

toTree :: [a] -> Tree a
toTree xs = fold Node (map Leaf xs)

toArray :: [a] -> Array Int a
toArray xs = listArray (0, length xs - 1) xs

tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate (u, v) f = array (u, v) [(i, f i) | i <- range (u, v)]

-- Solutions
-- Q1

-- Node (Node (Node L1 L2) L3) L4

depth :: Tree a -> Int
depth (Leaf _) = 0
depth (Node l r) = 1 + max (depth l) (depth r)

-- depth $ toTree xs = n - 1

{-
    T(n)
    = T(n - 1) + T(n - 1 ++) + T(1)
    = T(n - 1) + n - 1 + 1
    = T(n - 1) + n
    = T(n - 2) + n - 2 + 1 + n
    = T(n - 2) + n - 1 + n
    ...
    = 1 + 2 + ... + n
    = n(n + 1) / 2
    So O(n^2)
-}

fromTree' :: Tree a -> [a]
fromTree' = fromTree'' []

fromTree'' :: [a] -> Tree a -> [a]
fromTree'' xs (Leaf x) = x:xs
fromTree'' xs (Node l r) = fromTree'' (fromTree'' xs r) l

fold' :: (a -> a -> a) -> [a] -> a
fold' f (xs :: [a]) = go 0 (length xs)
    where
        array = toArray xs
        go i 1 = array ! i
        go i n = f (go i m) (go (i + m) (n - m))
            where
                m = n `div` 2

-- fold' Node [1, 2, 3, 4, 5]
-- go 0 5, m = 2
-- Node (go 0 2) (go 2 3)
-- Node (Node (go 0 1) (go 1 1)) (Node (go 2 1) Node (go 3 2)) 
-- Node (Node 1 2) (Node 3 (Node (go 3 1) (go 4 1)))
-- Node (Node 1 2) (Node 3 (Node 4 5))

fold'' :: (a -> a -> a) -> [a] -> a
fold'' f [x] = x
fold'' f xs = f (fold'' f lxs) (fold'' f rxs)
    where
        n = length xs
        (lxs, rxs) = splitAt (n `div` 2) xs

-- Q2
winning :: Int -> Int -> Bool
winning k p = k >= p || (not . and) pos
    where
        pos = map (winning k . (p -)) [1..k]

winning' :: Int -> Int -> Bool
winning' k p = table ! p
    where
        table = tabulate (0, p) memo
        memo :: Int -> Bool
        memo p' = k >= p' || (not . and) pos
            where
                pos = map ((table !) . (p' -)) [1..k]
-- Complexity: O(kp)

moves :: Int -> Int -> [Int]
moves k p = table ! p
    where
        table = tabulate (0, p) memo
        memo :: Int -> [Int]
        memo p'
            | k >= p' = [1..p']
            | otherwise = map fst pos'
                where
                    pos = map (\x -> (x, table ! (p' - x))) [1..k] 
                    pos' = filter (null . snd) pos

-- This works the same as winning however the base condition is changed as:
-- now the base winning contition is if 2 buckets are empty and you can empty
-- the final one (its balls <= k)
-- Our possibilities now include if we took 1 to k balls from any bucket instead
-- of just the bucket p

winning3 :: Int -> Int -> Int -> Int -> Bool
winning3 k p q r = table ! (p, q, r)
    where
        table = tabulate ((0, 0, 0), (p, q, r)) memo
        memo :: (Int, Int, Int) -> Bool
        memo (p', q', r') = lastPick || (not . and) pos
            where
                lastPick = (p' == 0 && q' == 0 && r' <= k) || (p' == 0 && q' <= k && r' == 0) || (p' <= k && q' == 0 && r' == 0)
                pos = concat [[table ! (p' - i, q', r') | i <- [1..k]],
                              [table ! (p', q' - i, r') | i <- [1..k]],
                              [table ! (p', q', r' - i) | i <- [1..k]]]

-- Complexity: O(kpqr)