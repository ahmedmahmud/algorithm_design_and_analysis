import Data.Array
import Data.List

data Path = Path Double [(Int, Int)]
    deriving (Show, Eq, Ord)

instance Num Path where
    Path d1 ps1 + Path d2 ps2 = Path (d1 + d2) (ps1 ++ ps2)
    Path d1 ps1 - Path d2 ps2 = Path (d1 - d2) (ps1 \\ ps2)
    fromInteger 0 = Path 0 []

tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate (u, v) f = array (u, v) [(i, f i) | i <- range (u, v)]

fromList :: [a] -> Array Int a
fromList xs = listArray (0, length xs - 1) xs

type Point = (Double, Double)

example :: Array Int Point
example = array (0, 3) (zip [0..] [(0, 0), (7, 7), (3, 4), (4, 3)])

dist :: Array Int Point -> Int -> Int -> Double
dist ar i j = sqrt (x * x + y * y) 
    where
        (xi, yi) = ar ! i
        (xj, yj) = ar ! j
        x = xi - xj
        y = yi - yj

bitonic'' :: (Int -> Int -> Double) -> Int -> Path
bitonic'' d n = table ! n where
    table = tabulate (0, n) mbitonic

    mbitonic :: Int -> Path
    mbitonic 0 = Path 0 [(0, 0)]
    mbitonic 1 = Path (2 * d 0 1) [(0, 1), (0, 1)]
    mbitonic n =
        minimum [ table ! k - d' (k - 1) k
        + d' (k - 1) n
        + sum [d' i (i + 1) | i <- [k .. n - 1]]
        | k <- [1 .. n - 1]]
        where
            d' :: Int -> Int -> Path
            d' i j = Path (d i j) [(min i j, max i j)]