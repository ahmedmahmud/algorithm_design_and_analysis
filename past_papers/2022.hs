import Data.Array

-- Helpers
hive :: [[Int]]
hive = [[9, 7, 5, 8, 1], [1, 2, 1, 1], [2, 1, 1], [1, 6], [1]]

tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate (u, v) f = array (u, v) [(i, f i) | i <- range (u, v)]

-- Solutions
-- Q1
fromHive :: [[Int]] -> Array (Int, Int) Int
fromHive hss = array ((0, 0), (n, n)) zipped
    where
        n = length (head hss) - 1
        hss' = map (\hs -> hs ++ replicate (n + 1 - length hs) 0) hss
        zipped = [((i, j), b) | (i, r) <- zip [0..] hss', (j, b) <- zip [0..] r]

bees :: [[Int]] -> Int
bees [] = 0
bees [[_]] = 0
bees hhs = min (head (last left) + bees left) (head (last right) + bees right)
    where
        hhs' = init hhs
        left = map init hhs'
        right = map tail hhs'

bees' :: [[Int]] -> Int
bees' hhs = table ! (n, 0)
    where
        n = length (head hhs) - 1
        beeCount = fromHive hhs
        table = tabulate ((0, 0), (n, n)) memo
        memo :: (Int, Int) -> Int
        memo (i, j)
            | i == 0 = 0
            | otherwise = min left right
            where
                left = beeCount ! (i - 1, j) + table ! (i - 1, j)
                right = beeCount ! (i - 1, j + 1) + table ! (i - 1, j + 1)
-- Complexity: O(nm)

-- Freezes because of left' -> right' -> left' ... case
bees'' :: [[Int]] -> Int
bees'' hhs = table ! (n, 0)
    where
        n = length (head hhs) - 1
        beeCount = fromHive hhs
        table = tabulate ((0, 0), (n, n)) memo
        memo :: (Int, Int) -> Int
        memo (i, j)
            | i == 0 = 0
            | j == 0 = minimum [left, right, right']
            | j == n = min left left'
            | otherwise = minimum [left, right, right', left']
            where
                left = beeCount ! (i - 1, j) + table ! (i - 1, j)
                right = beeCount ! (i - 1, j + 1) + table ! (i - 1, j + 1)
                left' = beeCount ! (i, j - 1) + table ! (i, j - 1)
                right' = beeCount ! (i, j + 1) + table ! (i, j + 1)

-- Q2
find :: [[a]] -> Int -> a
find xss k = concat xss !! k

toChunks :: [a] -> [[a]]
toChunks xs = toChunks' xs 1
    where
        toChunks' :: [a] -> Int -> [[a]]
        toChunks' [] _ = []
        toChunks' rxs i = axs : toChunks' bxs (i + 1)
            where
                (axs, bxs) = splitAt i rxs

-- Complexity:
-- O(n), n = length of xs

cons :: a -> [[a]] -> [[a]]
cons x xss = toChunks $ x : concat xss

-- Complexity:
-- O(n), n = total number of values in chunk array

find' :: [[a]] -> Int -> a
find' xss k = xss !! i !! j
    where
        (i, j) = find'' k 1
        find'' :: Int -> Int -> (Int, Int)
        find'' k' m
            | k' < m = (m - 1, k')
            | otherwise = find'' (k' - m) (m + 1)

-- Complexity:
-- O(sqrt n), n = total number of values in chunk array
-- O(m), m = number of chunks

type ChunkList a = [Queue a]

data Queue a = Queue [a] [a]

consQ :: a -> Queue a -> Queue a
consQ x (Queue as sb) = Queue (x:as) sb

-- C_lastQ (Queue as sb) = if length sb > 0 then 1 else length as
-- A_lastQ (Queue as sb) = 1
-- S_lastQ (Queue as sb) = length as

-- length as = k, length sb = 0 (worst case)
-- S_lastQ (Queue as sb) = k
-- S_lastQ (Queue as' sb') = 0
-- Plugging values into equality (C <= A + S_i - S_{i+1})
-- k <= 0 + k - 0

lastQ :: Queue a -> (Queue a, a)
lastQ (Queue [] []) = error "empty queue"
lastQ (Queue as []) = let (a:sa) = reverse as in (Queue [] sa, a)
lastQ (Queue as (b:sb)) = (Queue as sb, b)

-- Messy & poor solution
-- Doesn't work for if last chunk is not filled
cons' :: a -> ChunkList a -> ChunkList a
cons' x qs = new''
    where
        new = map lastQ qs
        lastChunk = [snd . last $ new]
        new' = zipWith (\(_, b) (q, _) -> consQ b q) new (drop 1 new)
        new'' = (Queue [x] [] : new') ++ [Queue lastChunk []]

-- xs = ([3], [1, 4], [1, 5, 9]) 1
-- new = ([], 3), ([1], 4) ([1, 5], 9)
-- lastChunk = [9]
-- new' = [3, 1] [4, 1, 5]
-- new'' = [1] [3, 1] [4, 1, 5] [9]