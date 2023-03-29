# 4. Lists

Haskell lists defined as:
```haskell
data [a] = [] | (:) a [a]
```

Think of `[a]` as a recursive data structure instead of an array.
```haskell
[1, 2, 3] = 1 : 2 : 3 : []
```

## Take `(++)`
```haskell
(++) :: [a] -> [a] -> [a]
[] ++ ys      = ys
(x: xs) ++ ys = x:(xs ++ ys)
```
The whole of the first list must be traversed so $T_{(++)}(n) \in O(n)$ where $n$ is `length xs`

## Take `foldr` and `foldl`
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f k [] = k
foldr f k (x:xs) = f x (foldr f k xs)
```
This expands to:  
`foldr f k [x1, x2, ..., xn] = f x1 (f x2 (... (f xn k)))`
We fold from right to left running `f` with the accumalated result where `k` is the initial value for this accumalator.  
If `f` is a binop `o` then:  
`foldr (o) k [x1, .., xn] = x1 o (x2 o (... o (xn o k)))`  
and if `o` is associative  
`foldr (o) k [x1, .., xn] = x1 o x2 o ... o xn o k`  
### Example: `concat`
```
concat (xs:xss) = xs ++ concat xss
```
Which expands to:  
`xs1 ++ (xs2 ++ (... ++ (xsn ++ [])))`  
So we can write concat in terms of `foldr`
```
concatr :: [[a]] -> [a]
concatr = foldr (++) []
```
This has complexity $O(mn)$ where $m$ is length of each child array and $n$ is the length of the parent array.  
If we instead took foldl as `++` is associative
```
concatl :: [[a]] -> [a]
concatl = foldl (++) []
```
This expands to:
`((([] ++ x1) ++ x2) ++ ...) ++ xn`  
However the way `++` we know that the complexity of this is:
$$
0 + m + 2m + 3m + ... + (n - 1)m \\
m\sum_{i=1}^{n-1} i \\
\text{So }O(n^2m)\text{ as degree of linear sum is 2}
$$
With this we know that order matters.  
This is called being $extensionally$ equal but $intensionally$ different. They produce same values in different ways  
The issue we face is left-associated lists suffer from poor time complexity so we must change the way we represent them

# Abstract Datatypes
```
class List list where
    toList :: list a -> [a]
    fromList :: [a] -> list a
```
Properly defined when
```
toList o fromList = id
```
holds. Establishes an abstract list should not be modified when passing through its representation. `toList` is an example of an $abstraction$ function: it takes the ocncrete implementation to its abstract representation.  
`fromList` is a $representation$ function as it determines a concrete representation of the abstract type.


As function composition is right associative:
`((zs++) o (ys++) o (xs++)) [] = zs ++ (ys ++ (xs ++ []))`
So we define `DList a` that:  
![](2023-03-20-23-36-55.png)