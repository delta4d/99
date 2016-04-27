-- 99 haskell problem personal solution
-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
-- there is no invalid input checking
-- some solutions are not very efficient
-- some solutions seem messy
-- <delta> delta4d@gmail.com

module Prob where

import System.Random (getStdGen, random)
import Data.List (sortBy, group, groupBy, minimumBy, delete, intercalate, nub, sort, permutations, transpose)
import Data.Ord (comparing)
import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import Data.Function (on)
import Control.Monad (guard)
import Text.ParserCombinators.Parsec

------------------------------ 1-10 Lists ------------------------------------

-- 1. last element of a list

myLast :: [a] -> a
myLast [] = error "empty list!"
myLast [x] = x
myLast (_:xs) = myLast xs


-- 2. last but one element of a list

myButLast :: [a] -> a
myButLast [] = error "no but last!"
myButLast [x] = error "no but last!"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs


-- 3. kth element of a list

elementAt :: [a] -> Int -> a
elementAt [] k = error "too short!"
elementAt (x:xs) k | k == 1    = x
                   | otherwise = elementAt xs (k-1)


-- 4. length of list

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = myLength xs + 1


-- 5. reverse a list

myReverse :: [a] -> [a]
myReverse x = myReverse' x []
  where
    myReverse' [] y = y
    myReverse' (x:xs) y = myReverse' xs (x:y)


-- 6. judge palindrome

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = myReverse x == x


-- 7. flatten list

data NestedList a = Elem a
                  | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)


-- 8. eliminate consecutive duplicates

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs@(y:_)) | x == y    = compress xs
                      | otherwise = x : compress xs


-- 9. pack consecutive duplicates

pack :: (Eq a) => [a] -> [[a]]
pack x = pack' x []
  where
    pack' [] y = myReverse y
    pack' (x:xs) [] = pack' xs [[x]]
    pack' (x:xs) zs@(ys@(y:_):z) | x == y    = pack' xs ((x:ys):z)
                                 | otherwise = pack' xs ([x]:zs)


-- 10. encoding list

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (myLength x, head x)) . pack

-------------------- 11-20 Lists, continued ----------------------------------

-- 11. modified run-length encoding

data ListItem a = Multiple Int a
                | Single a
                deriving (Show, Eq)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map (\(l, c) -> if l == 1 then Single c else Multiple l c) . encode


-- 12. decode a run-length encoded list

decodeModified :: [ListItem a] -> [a]
decodeModified [] = []
decodeModified ((Single c):x) = c : (decodeModified x)
decodeModified ((Multiple l c):x)
    | l == 1    = c : (decodeModified x)
    | otherwise = c : (decodeModified ((Multiple (l-1) c):x))


-- 13. direct encode

encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect x = encodeDirect' x []
  where
    encodeDirect' [] y = myReverse y
    encodeDirect' (x:xs) [] = encodeDirect' xs [Single x]
    encodeDirect' (x:xs) z@((Single y):ys)
        | x == y    = encodeDirect' xs ((Multiple 2 y):ys)
        | otherwise = encodeDirect' xs ((Single x):z)
    encodeDirect' (x:xs) z@((Multiple l y):ys)
        | x == y    = encodeDirect' xs ((Multiple (l+1) y):ys)
        | otherwise = encodeDirect' xs ((Single x):z)


-- 14. duplicate elemtns of a list

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)


-- 15. replicate elements of a list

repli :: [a] -> Int -> [a]
repli x n = repli' x n
  where
    repli' [] _ = []
    repli' (x:xs) 1 = x : (repli' xs n)
    repli' xs@(x:_) m = x : (repli' xs (m-1))


-- 16. drop every nth element

dropEvery :: [a] -> Int -> [a]
dropEvery x n = dropEvery' x 1 n
  where
    dropEvery' [] _ _ = []
    dropEvery' (x:xs) m n | m == n    = dropEvery' xs 1 n
                          | otherwise = x : (dropEvery' xs (m+1) n)


-- 17. split list into 2 parts

split :: [a] -> Int -> ([a], [a])
split x 0 = ([], x)
split (x:xs) n = let (p, q) = split xs (n-1)
                 in  (x:p, q)


-- 18. extract a slice from a list

slice :: [a] -> Int -> Int -> [a]
slice (x:xs) st ed | st > 1    = slice xs (st-1) (ed-1)
                   | st == 1   = x : (slice xs 0 (ed-1))
                   | ed > 0    = x : (slice xs st (ed-1))
                   | otherwise = []


-- 19. rotate list

rotate :: [a] -> Int -> [a]
rotate x k = let (a, b) = split x k
             in  b ++ a


-- 20. remove kth element from a list

removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = let (p, q) = removeAt (n-1) xs
                    in  (p, x:q)

------------------------- 21-28 Lists again -----------------------------------

-- 21. insert at given pos

insertAt :: a -> [a] -> Int -> [a]
insertAt c x 1 = c:x
insertAt c (x:xs) p = x : insertAt c xs (p-1)


-- 22. generate list

range :: Int -> Int -> [Int]
range st ed | st > ed   = []
            | otherwise = st : (range (st+1) ed)


-- 23. random choose elements from a list

rnd_select :: [a] -> Int -> IO [a]
rnd_select x n = do
    rng <- getStdGen
    return $ rnd_select' rng x (length x) n
      where
        rnd_select' _ [] _ _ = []
        rnd_select' g (x:xs) l n =
            let (k', g') = random g
                k = k' `mod` l
            in  if k < n
                then x : (rnd_select' g' xs (l-1) (n-1))
                else rnd_select' g' xs (l-1) n


-- 24. random choose elements from a range

diff_select :: Int -> Int -> IO [Int]
diff_select m n = rnd_select (range 1 n) m


-- 25. generate a random permutation

rnd_permu :: [a] -> IO [a]
rnd_permu x = do
    g <- getStdGen
    return $ rnd_permu' g x (length x)
      where
        rnd_permu' _ [] _ = []
        rnd_permu' g x l =
            let (k, g') = random g
                k' = k `mod` l + 1
                (y, x') = removeAt k' x
                l' = l - 1
            in  y : (rnd_permu' g' x' l')


-- 26. generate all k combinations of n distinct objects

combinations :: Int -> [a] -> [[a]]
combinations n x = c n x [] []
  where
    c 0 _ cur ret = reverse cur : ret
    c _ [] _ ret = ret
    c r (x:xs) cur ret = (c (r-1) xs (x:cur) ret) ++ (c r xs cur ret)


-- 27a. 9 to [2, 3, 4]

group3 = group' [2, 3, 4]


-- 27b. n to [...]

group' :: [Int] -> [a] -> [[[a]]]
group' [] [] = [[]]
group' (n:ns) x = concat $ map (\(grp, left) -> map (grp :) (group' ns left)) (combo n x)
  where
    combo :: Int -> [a] -> [([a], [a])]
    combo 0 x = [([], x)]
    combo _ [] = []
    combo n (x:xs) = select ++ unselect
      where
        select   = map (\(grp, left) -> (x:grp, left)) (combo (n-1) xs)
        unselect = map (\(grp, left) -> (grp, x:left)) (combo n xs)


-- 28a. sort by length

lsort :: (Ord a) => [[a]] -> [[a]]
lsort = sortBy (\x y -> let lx = length x
                            ly = length y
                        in  if lx == ly
                            then x `compare` y
                            else lx `compare` ly)


-- 28b. sort by length frequency

lfsort :: (Ord a) => [[a]] -> [[a]]
lfsort = concat . lsort . groupBy (\x y -> length x == length y) . lsort

------------------------- 31-41 Arithmetic -----------------------------------

-- 31. judge prime

isPrime :: Int -> Bool
isPrime n | n <= 1 = False
          | otherwise = _is_p 2 n
            where
              _is_p x n | x * x > n      = True
                        | n `mod` x == 0 = False
                        | otherwise      = _is_p (x+1) n


-- 32. calc gcd

myGCD :: Int -> Int -> Int
myGCD a b | a < 0 || b < 0 = myGCD (abs a) (abs b)
          | b == 0         = a
          | otherwise      = myGCD b (a `mod` b)


-- 33. judge coprime

coprime :: Int -> Int -> Bool
coprime a b = myGCD a b == 1


-- 34. euler totient phi

totient :: Int -> Int
totient n = length [ x | x <- [1..n-1], coprime x n ]


-- 35. factor a number

primeFactors :: Int -> [Int]
primeFactors n = _p n 2
  where
    _p 1 _ = []
    _p n p | n `mod` p == 0 = p : _p (n `div` p) p
           | otherwise      = _p n (p+1)


-- 36. factor a number

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult = map (\(x, y) -> (y, x)) . encode . primeFactors


-- 37. euler phi

phi :: Int -> Int
phi = foldl (\acc (p, n) -> acc*(p-1)*p^(n-1)) 1 . prime_factors_mult


-- 38. compare 34, 37
--   34: O(nlgn)
--   37: O(n^0.5)


-- 39. list primes

primesR :: Int -> Int -> [Int]
primesR st ed = filter isPrime [st..ed]


-- 40. goldbach solution

goldbach :: Int -> (Int, Int)
goldbach n = head [ (x, n-x) | x <- [1..n], isPrime x && isPrime (n-x) ]


-- 41a. goldbach list

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList st ed | st < 4 || odd st = goldbachList (st+1) ed
                   | st > ed = []
                   | otherwise = goldbach st : goldbachList (st+2) ed


-- 41b. goldbach list

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' st ed mn
    | st < 4 || odd st = goldbachList' (st+1) ed mn
    | st > ed = []
    | otherwise = cnt
      where
        n   = st
        sol = [ (x, n-x) | x <- [mn..n-mn], isPrime x && isPrime (n-x) ]
        nxt = goldbachList' (st+2) ed mn
        cnt = if null sol
              then nxt
              else head sol : nxt

-------------------------- 46-50 Logic and codes ------------------------------

-- 46. truth table 1

and' = (&&)
or'  = (||)
equ' :: Bool -> Bool -> Bool
equ' a b = a == b
infixl 5 `and'`
infixl 4 `or'`
infixl 3 `equ'`

table :: (Bool -> Bool -> Bool) -> [[Bool]]
table e = [ [x, y, e x y] | x <- [True, False], y <- [True, False] ]


-- 47. truth table 2

table2 = table


-- 48. truth table 3

tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n e = _t [] n e
  where
    _t a 0 e = let b = reverse a in [reverse ((e b):a)]
    _t a n e = _t (True:a) (n-1) e ++ _t (False:a) (n-1) e


-- 49. gray code

gray :: Int -> [String]
gray 0 = [""]
gray n = map ('0' :) g ++ map ('1' :) g
  where g = gray (n-1)


-- 50. huffman code

huffman :: [(Char, Int)] -> [(Char, String)]
huffman = h . map (\(c, f) -> (f, [(c, "")]))
  where
    h [(_, r)] = sortBy (comparing fst) r
    h x = let ((a,b):(c,d):e) = sortBy (comparing fst) x
              b' = map (\(x, y) -> (x, '0':y)) b
              d' = map (\(x, y) -> (x, '1':y)) d
          in  h $ (a+c, b'++d') : e

------------------------ 54A-60 Binary trees ----------------------------------

data Tree a = Empty
            | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

insertBST :: (Ord a) => a -> Tree a -> Tree a
insertBST x Empty = Branch x Empty Empty
insertBST x (Branch y l r) =
    if x < y
    then Branch y (insertBST x l) r
    else Branch y l (insertBST x r)

flipTree :: Tree a -> Tree a
flipTree Empty = Empty
flipTree (Branch x l r) = Branch x (flipTree r) (flipTree l)

heightTree :: Tree a -> Int
heightTree Empty = 0
heightTree (Branch _ l r) = 1 + max (heightTree l) (heightTree r)

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ l r) = 1 + countNodes l + countNodes r

-- 54A. thanks to haskell's type system


-- 55. construct completely balanced binary trees

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n =
    let m = (n-1) `div` 2
        left = cbalTree m
        right = cbalTree (n-1-m)
        lr = [Branch 'x' l r | l <- left, r <- right]
        rl = [Branch 'x' l r | l <- right, r <- left]
    in  if odd n
        then lr
        else lr ++ rl


-- 56. check symmetric binary tree

symmetric :: (Eq a) => Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = _s l r
  where
    _s Empty Empty = True
    _s (Branch x1 l1 r1) (Branch x2 l2 r2) = x1 == x2 && _s l1 r2 && _s r1 l2
    _s _ _ = False


-- 57. construct binary search tree

construct :: (Ord a) => [a] -> Tree a
construct [] = Empty
construct (x:xs) = _c xs (Branch x Empty Empty)
  where
    _c [] t = t
    _c (x:xs) t = _c xs (insertBST x t)


-- 58. construct symmetric, completely balanced binary tree

symCbalTrees :: Int -> [Tree Char]
symCbalTrees 0 = [Empty]
symCbalTrees n = if even n then [] else zipWith (Branch 'x') t t'
  where
    t  = cbalTree (n `div` 2)
    t' = map flipTree t


-- 59. construct height-balanced binary trees with given element and height

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x h =
    let t1 = hbalTree x (h-1)
        t2 = hbalTree x (h-2)
        z1 = [ Branch x l r | l <- t1, r <- t2 ]
        z2 = [ Branch x l r | l <- t1, r <- t1 ]
        z3 = [ Branch x l r | l <- t2, r <- t1 ]
    in  z1 ++ z2 ++ z3


-- 60. construct height-balanced binary trees with given element and nodes number

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes x n =
    let ts = [ (l, r) | m <- [0..n-1],
                        l <- hbalTreeNodes x m,
                        r <- hbalTreeNodes x (n-1-m),
                        abs (heightTree l - heightTree r) <= 1 ]
    in  map (\(a, b) -> Branch x a b) ts

---------------------- 61-69 Binary trees, continued --------------------------

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

-- 61. count leaves

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r


-- 61A. collect leaves

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r


-- 62. collect internal nodes

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = x : internals l ++ internals r


-- 62B. collect nodes at given level

atLevel :: Tree a -> Int -> [a]
atLevel = _al 1
  where
    _al _ Empty _ = []
    _al d (Branch x l r) le | d == le = [x]
                            | otherwise = _al (d+1) l le ++ _al (d+1) r le


-- 63. construct complete binary tree

completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree n = Branch 'x' (completeBinaryTree a) (completeBinaryTree b)
  where
    b' = last $ takeWhile (\x -> 2*x+1 <= n) [ 2^e-1 | e <- [0..] ]
    a' = n - 1 - b'
    r = n - 1 - b' - b'
    (a, b) = if r <= b' + 1 then (a', b') else (n-r, r-1)

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree t =
    let h = heightTree t
        isLeft Empty = (True, 0)
        isLeft (Branch _ l r) =
            let (okl, hl) = isLeft l
                (okr, hr) = isLeft r
                h = (max hl hr) + 1
            in  (okl&&okr&&hl>=hr, h)
    in  and [ 2^(l-1) == length (atLevel t l) | l <- [1..h-1] ] && fst (isLeft t)


-- 64. layout

layout64 :: Tree a -> Tree (a, (Int, Int))
layout64 t = _l t 1 1
  where
    _l Empty _ _ = Empty
    _l (Branch x l r) d i = Branch (x, (i', d)) left right
      where
        left = _l l (d+1) i
        i' = countNodes left + 1
        right = _l r (d+1) (i'+1)


-- 65. layout

layout65 :: Tree a -> Tree (a, (Int, Int))
layout65 t = _l t step (leftLength t step) 1
  where
    h = heightTree t
    step = if h < 2 then 0 else 2^(h-2)
    leftLength Empty _ = 0
    leftLength (Branch _ l _) step = step + leftLength l (step `div` 2)
    _l Empty _ _ _ = Empty
    _l (Branch v l r) step x y = Branch (v, (x, y)) left right
      where
        s = step `div` 2
        left = _l l s (x-step) (y+1)
        right = _l r s (x+step) (y+1)


-- 66. layout

layout66 :: (Eq a, Show a) => Tree a -> Tree (a, (Int, Int))
layout66 t = fst $ layout t
  where
    layout Empty = (Empty, ([], []))
    layout (Branch x Empty Empty) = (Branch (x, (1, 1)) Empty Empty, ([1], [1]))
    layout (Branch x left right) = (t, (l, r))
      where
        offset _ Empty = Empty
        offset d (Branch (v, (x, y)) left right) =
            Branch (v, (x+d, y+1)) left' right'
              where left'  = offset d left
                    right' = offset d right
        collect Empty = ([], [])
        collect (Branch (_, (x, _)) left right) = (x:l, x:r)
            where (l1, r1) = collect left
                  (l2, r2) = collect right
                  l = _c min l1 l2
                  r = _c max r1 r2
                  _c _ [] x = x
                  _c _ x [] = x
                  _c f (x:xs) (y:ys) = f x y : _c f xs ys
        t = if right == Empty
            then let (left'@(Branch (_, (lx, _)) _ _), _) = layout left
                     (right', _) = layout right
                 in  Branch (x, (lx+1, 1)) (offset 0 left') right'
            else if left == Empty
                 then let (right'@(Branch (_, (rx, _)) _ _), _) = layout right
                          (left', _) = layout left
                          d = if rx - 1 < 1 then 2 - rx else 0
                      in  Branch (x, (rx-1+d, 1)) left' (offset d right')
                 else let (t1, (l1, r1)) = layout left
                          (t2, (l2, r2)) = layout right
                          min_d = minimum $ zipWith (-) l2 r1
                          off_r = 2 - min_d
                          off_l = 1 - minimum l1
                          left' = offset off_l t1
                          right' = offset (off_r + off_l) t2
                          (Branch (_, (x1, _)) _ _) = left'
                          (Branch (_, (x2, _)) _ _) = right'
                          root_x = (x1 + x2) `div` 2
                      in  Branch (x, (root_x, 1)) left' right'
        (l, r) = collect t


-- 67. binary trees string representation

treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) = x : '(' : treeToString l ++ "," ++ treeToString r ++ ")"

stringToTree :: String -> Tree Char
stringToTree s = case parse parseTree "" s of
    Left err -> error (show err)
    Right tr -> tr
  where
    parseTree = try node <|> try leaf <|> return Empty
      where
        node = do
            x <- letter; char '('
            l <- parseTree; char ','
            r <- parseTree; char ')'
            return $ Branch x l r
        leaf = letter >>= \x -> return $ Branch x Empty Empty


-- 68. preorder, inorder, tree reconstruction

treeToPreorder :: Tree a -> [a]
treeToPreorder Empty = []
treeToPreorder (Branch x l r) = x : treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree a -> [a]
treeToInorder Empty = []
treeToInorder (Branch x l r) = treeToInorder l ++ [x] ++ treeToInorder r

preInTree :: (Eq a) => [a] -> [a] -> Tree a
preInTree [] [] = Empty
preInTree (x:xs) y = Branch x (preInTree lx ly) (preInTree rx ry)
  where
    (ly, _:ry) = break (== x) y
    (lx, rx) = splitAt (length ly) xs


-- 69. binary tree dot string representation

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch x l r) = x : tree2ds l ++ tree2ds r

ds2tree :: String -> Tree Char
ds2tree = fst . _d
  where
    _d [] = (Empty, [])
    _d (x:xs) = if x == '.'
                then (Empty, xs)
                else (Branch x left right, zs)
                  where
                    (left, ys) = _d xs
                    (right, zs) = _d ys

-------------------------- 70B-73 Multiway trees ------------------------------

data MTree a = Node a [MTree a]
            deriving (Eq, Show)

mtree1 = Node 'a' []
mtree2 = Node 'a' [Node 'b' []]
mtree3 = Node 'a' [Node 'b' [Node 'c' []]]
mtree4 = Node 'b' [Node 'd' [], Node 'e' []]
mtree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]


-- 70B. thanks to haskell's type system


-- 70C. count the nodes of a multiway tree

nnodes :: MTree a -> Int
nnodes (Node _ xs) = 1 + (sum $ map nnodes xs)


-- 70. tree construction from a node string

mtreeToString :: MTree Char -> String
mtreeToString (Node x xs) = x : (concatMap mtreeToString xs) ++ "^"

stringToMtree :: String -> MTree Char
stringToMtree = fst . _s
  where
    _s (x:'^':xs) = (Node x [], xs)
    _s (x:xs) = (Node x childs, left)
      where
        (childs, left) = _c xs
        _c ('^':xs) = ([], xs)
        _c xs = (c:cs, r)
          where
            (c, r') = _s xs
            (cs, r) = _c r'


-- 71. determine the internal path length of a tree

ipl :: MTree a -> Int
ipl = fst . _i
  where
    _i (Node _ []) = (0, 1)
    _i (Node _ xs) = (i+n, 1+n)
      where
        (i, n) = foldl1 (\(a, b) (c, d) -> (a+c, b+d)) (map _i xs)


-- 72. bottom up seq

bottom_up :: MTree Char -> String
bottom_up (Node x []) = [x]
bottom_up (Node x xs) = concatMap bottom_up xs ++ [x]


-- 73. lispy tree repr

display_lisp :: MTree Char -> String
display_lisp (Node x []) = [x]
display_lisp (Node x xs) = '(' : x : ' ' : unwords (map display_lisp xs) ++ ")"

--------------------------------- 80-89 Graphs --------------------------------

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)
data Adj a   = Adj [(a, [a])] deriving (Show, Eq)
data Edge a  = Edge [(a, a)] deriving (Show, Eq)

-- 80. graph conversion

graphToAdj :: (Eq a) => Graph a -> Adj a
graphToAdj (Graph p e) = Adj $ map (\x -> (x, _g x e)) p
  where
    _g x = concatMap (\(a, b) -> if a == x
                                 then [b]
                                 else if b == x
                                      then [a]
                                      else [])

graphToEdge :: (Eq a) => Graph a -> Edge a
graphToEdge (Graph p q) = Edge (q ++ r)
  where
    r = let g = filter (\x -> all (\(a, b) -> a /= x && b /= x) q) p
        in  zip g g

adjToGraph :: (Eq a) => Adj a -> Graph a
adjToGraph (Adj xs) = Graph p q
  where
    p = map fst xs
    q' = concatMap (\(x, ys) -> map (\y -> (x, y)) ys) xs
    q = removeDup q'
    removeDup [] = []
    removeDup ((a,b):c) = if (a, b) `elem` c || (b, a) `elem` c
                          then removeDup c
                          else (a, b) : removeDup c

adjToEdge :: (Eq a) => Adj a -> Edge a
adjToEdge = graphToEdge . adjToGraph

edgeToGraph :: (Eq a) => Edge a -> Graph a
edgeToGraph (Edge e) = Graph p q
  where
    p = removeDup $ concatMap (\(a, b) -> [a, b]) e
    removeDup [] = []
    removeDup (x:xs) = if x `elem` xs
                          then removeDup xs
                          else x : removeDup xs
    q = filter (\(a, b) -> a /= b) e

edgeToAdj :: (Eq a) => Edge a -> Adj a
edgeToAdj = graphToAdj . edgeToGraph


-- 81. path from one node to another node

paths :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths st ed edge = _p st ed edge []
  where
    _p cur ed edge vis | cur == ed = [[cur]]
                       | otherwise = concatMap (\x -> map (cur :) $ _p x ed edge (x:vis)) nxts
      where
        nxts = map snd $ filter (\(a, b) -> a == cur && not (b `elem` vis)) edge


-- 82. cycle from a given node

cycles :: (Eq a) => a -> [(a, a)] -> [[a]]
cycles st e = concatMap (\x -> map (st :) $ paths x st e) nxts
  where
    nxts = concatMap (\(a, b) -> if a == st then [b] else []) e


-- 83. construct all spanning trees

isTree :: (Eq a) => [a] -> [(a, a)] -> Bool
isTree p e = length p == length e + 1 && isConn p e

isConn :: (Eq a) => [a] -> [(a, a)] -> Bool
isConn p@(st:_) e = length (_ic st [st]) == length p
  where
    _ic c p = if null nxts
              then p
              else foldl f p nxts
      where
        f vis st = if st `elem` vis then vis else _ic st (st:vis)
        nxts' = concatMap (\(a, b) -> if a == c
                                      then [b]
                                      else if b == c then [a] else []) e
        nxts = filter (\x -> not $ x `elem` p) nxts'

spantree :: (Eq a) => Graph a -> [Graph a]
spantree g@(Graph p' _) = _s g [] []
  where
    n = length p'
    _ok p e = isTree p e && length p == n
    _s (Graph _ []) ps es = if _ok ps es then [Graph ps es] else []
    _s (Graph p (e@(a, b):f)) ps es
        | _ok ps es = [Graph ps es]
        | otherwise = _s (Graph p f) ps' (e:es) ++ _s (Graph p f) ps es
      where
        ps'' = if a `elem` ps then ps else a:ps
        ps' = if b `elem` ps'' then ps'' else b:ps''


-- 84. construct the minimal spanning tree

inf = maxBound :: Int

prim :: (Eq a) => [a] -> [(a, a, Int)] -> [(a, a, Int)]
prim (p:ps) e = init $ _p dis []
  where
    p1 (x, _, _) = x
    p3 (_, _, x) = x
    dis = (p, p, 0) : map (\x -> (x, x, inf)) ps
    _p [] s = s
    _p d s = _p d' s'
      where
        node = minimumBy (comparing p3) d
        s' = node : s
        d' = upd e (p1 node) (delete node d)
        upd [] _ d = d
        upd ((u,v,w):es) i d
            | u /= i && v /= i = upd es i d
            | otherwise = let j = if u == i then v else u
                              _upd j w [] = []
                              _upd j w d@(n@(i',j',w'):ds)
                                | i' == j = if w < w' then (i', i, w):ds else d
                                | otherwise = n : _upd j w ds
                          in  upd es i (_upd j w d)


-- 85. graph isomorphism

iso :: (Eq a) => Graph a -> Graph a -> Bool
iso (Graph a1 e1) (Graph a2 e2) = length a1 == length a2 && length e1 == length e2 && _iso (permutations a1)
  where
    _iso ps = or [ _ok (zip a1 p) e1 | p <- ps ]
    _ok _ [] = True
    _ok mp ((x, y):es) =
        let val i = snd . head . dropWhile ((/= i) . fst) $ mp
            x' = val x
            y' = val y
        in  if (x', y') `elem` e2
            then _ok mp es
            else False


-- 86. node degree and graph coloration

degree :: (Eq a) => Graph a -> [(a, Int)]
degree g = let Adj a = graphToAdj g
           in  map (\(x, y) -> (x, length y)) a

sortDegree :: (Eq a) => Graph a -> [a]
sortDegree = map fst . sortBy (flip . comparing $ snd) . degree

kcolor :: (Eq a) => Graph a -> [(a, Int)]
kcolor g = _kcolor (sortDegree g) [] e
  where
    Adj e = graphToAdj g
    _kcolor [] r _ = []
    _kcolor (x:xs) vis e = (x, c) : _kcolor xs ((x, c):vis) e
      where
        (_, nei) = head $ dropWhile ((x /=) . fst) e
        vs = map fst vis
        cnei = filter (\x -> x `elem` vs) nei
        cs = map (\x -> snd $ head $ dropWhile ((x /=) . fst) vis) cnei
        c = head [ x | x <- [1..], x `notElem` cs ]


-- 87. dfs graph

depthfirst :: (Eq a) => ([a], [(a, a)]) -> a -> [a]
depthfirst (p, e) st = fst $ _dfs st [st]
  where
    _dfs cur vis =
        if null nxts
        then ([cur], vis)
        else (cur:s, vis')
          where
            (s, vis') = foldl (\(s, v) n -> if n `elem` v
                                            then (s, v)
                                            else let (s', v') = _dfs n (n:v)
                                                 in  (s++s', v')) ([], vis) nxts
            nxts = concatMap (\(a, b) -> if a == cur
                                         then if b `elem` vis then [] else [b]
                                         else if b == cur
                                              then if a `elem` vis then [] else [a]
                                              else []) e


-- 88. connected components

connectedcomponents :: (Eq a) => ([a], [(a, a)]) -> [[a]]
connectedcomponents = _c []
  where
    _c r ([], e) = reverse r
    _c r (v:vs, e) = _c r' (v', e)
      where
        r' = depthfirst (v:vs, e) v : r
        v' = foldl (\v x -> delete x v) vs [ p | ps <- r', p <- ps ]


-- 89. bipartite graphs

bipartite :: (Eq a) => Graph a -> Bool
bipartite g = length (onlyColor $ kcolor g) == 2
  where
    onlyColor = nub . sort . map snd


------------------------- 90-94 Miscellaneous problems ------------------------

-- 90. eight queens problem

queens :: Int -> [[Int]]
queens n = filter isValid . permutations $ [1..n]
  where
    isValid p = and [ _ok p x y | x <- [1..n], let y = p!!(x-1) ]
    _ok p x y = and [ abs (x-x') /= abs (y-y') | x' <- [1..n], x' /= x, let y' = p!!(x'-1) ]


-- 91. knight's tour

knightsTo :: Int -> (Int, Int) -> [[(Int, Int)]]
knightsTo n st = dfs (n*n) [[st]]
  where
    dfs 1 x = x
    dfs i x = dfs (i-1) $ concatMap nxts x
    nxts vis@(x:xs) = map (: vis) $ sortBy (comparing out) (jumpFrom x)
      where
        jumpFrom (x, y) = [ (x+i, y+j) | i <- [-2..2], j <- [-2..2]
                                       , abs i + abs j == 3
                                       , i /= 0, j /= 0
                                       , x + i >= 1, x + i <= n
                                       , y + j >= 1, y + j <= n
                                       , (x+i, y+j) `notElem` vis ]
        out = length . jumpFrom

closedKnights :: Int -> [[(Int, Int)]]
closedKnights n = dfs (n*n) [[(1, 1)]]
  where
    dfs 1 x = x
    dfs i x = dfs (i-1) $ concatMap nxts x
      where
        nxts vis@(x:xs) = map (: vis) $ _f $ sortBy (comparing out) (jumpFrom x)
          where
            jumpFrom (x, y) = [ (x+i, y+j) | i <- [-2..2], j <- [-2..2]
                                           , abs i + abs j == 3
                                           , i /= 0, j /= 0
                                           , x + i >= 1, x + i <= n
                                           , y + j >= 1, y + j <= n
                                           , (x+i, y+j) `notElem` vis ]
            out = length . jumpFrom
            _f x = if (2, 3) `elem` x && i > 2
                   then delete (2, 3) x
                   else x


-- 92. von koch's conjecture

vonKoch :: [(Int, Int)] -> [[Int]]
vonKoch e = filter _ok (permutations [1..n])
  where
    n = length e + 1
    _ok f = (length . nub . sort . map (\(x, y) -> abs (f!!(x-1) - f!!(y-1)))) e == n - 1


-- 93. an arithmetic puzzle

data Expr = Bin Char Expr Expr
          | Val Int

-- always add ()
instance Show Expr where
    show (Val x) = show x
    show e@(Bin op e1 e2) = "(" ++ show e1 ++ [op] ++ show e2 ++ ")"

eval :: Char -> Rational -> Rational -> Rational
eval '+' x y = x + y
eval '-' x y = x - y
eval '*' x y = x * y
eval '/' x y = x / y

exprs :: [Int] -> [(Expr, Expr)]
exprs xs = do
    (a1, a2) <- split2 xs
    (e1, v1) <- search a1
    (e2, v2) <- search a2
    guard $ v1 == v2
    return $ (e1, e2)

split2 :: [Int] -> [([Int], [Int])]
split2 (x:xs) = _s [x] xs
  where
    _s xs [y] = [(reverse xs, [y])]
    _s xs (y:ys) = (reverse xs, y:ys) : _s (y:xs) ys

search :: [Int] -> [(Expr, Rational)]
search [x] = [(Val x, fromIntegral x)]
search xs = do
    (a1, a2) <- split2 xs
    (e1, v1) <- search a1
    (e2, v2) <- search a2
    op <- "+-*/"
    guard $ op /= '/' || v2 /= 0
    return (Bin op e1 e2, eval op v1 v2)

puzzle :: [Int] -> [String]
puzzle = map (\(e1, e2) -> show e1 ++ " = " ++ show e2) . exprs


-- 94. generate k-regular simple graphs with n nodes

-- sloooooooooooow
regular :: Int -> Int -> [[(Int, Int)]]
regular n k = uniq . filter is_regular $ com edges (n*k `div` 2)
  where
    vertices = [1..n]
    edges = [ (i, j) | i <- vertices, j <- vertices, i /= j ]
    com xss@(x:xs) k | k == 0 = [[]]
                     | length xss == k = [xss]
                     | otherwise = map (x :) (com xs (k-1)) ++ com xs k
    uniq [] = []
    uniq (x:xs) = if or [ iso (Graph vertices x) (Graph vertices y) | y <- xs ]
                  then uniq xs
                  else x : uniq xs
    is_regular es = and [ d == k | (_, d) <- degree (Graph vertices es) ]


-------------------- 95-99 Miscellaneous problems, continued ------------------

-- 95. english number words

fullWords :: Int -> String
fullWords = intercalate "-" . map (\x -> ewords !! (digitToInt x)) . show
  where ewords = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]


-- 96. syntax checker

identifier :: String -> Bool
identifier s = case parse parseIden "" s of
    Left err -> False
    Right i -> True
  where
    ld = many1 $ letter <|> digit
    mld = char '-' >> ld >>= return . ('-' :)
    parseIden = do
        a <- letter
        b <- many $ mld <|> ld
        return $ a : concat b


-- 97. sudoku

-- sloooooooooooow
sudoku :: [[Int]] -> [[[Int]]]
sudoku b = map toBoard $ dfs [foldl trans (const xs) . zip idxs . concat $ b]
  where
    xs = [0..8]
    trans f (p, v) = \q -> if q == p && v > 0 then [v] else f q
    toBoard b = [ [ head (b (i, j)) | j <- xs ] | i <- xs ]
    idxs = [ (i, j) | i <- [0..8], j <- [0..8] ]
    dfs boards = foldl f boards idxs
      where
        f bs p@(x, y) = [ g b q | b <- bs, q <- b p ]
          where
            x' = x `div` 3
            y' = y `div` 3
            g b a = \q@(i, j) ->
                if q == p
                then [a]
                else if i == x || j == y || x' == i `div` 3 && y' == j `div` 3
                     then delete a (b q)
                     else b q


-- 98. nonograms

-- sloooooooooooow
nonogram :: [[Int]] -> [[Int]] -> [String]
nonogram r c = map unlines $ filter (ok . transpose) $ _perm r (length c)
  where
    ok = (c == ) . map (filter (> 0) . map (\g -> if head g == 'X' then length g else 0) . group)
    _perm [] _ = [[]]
    _perm (x:xs) n = (:) <$> _p x (length x) (sum x) n <*> _perm xs n
       where
         _p [] _ _ n = [replicate n ' ']
         _p xss@(x:xs) m r n = put ++ unput
           where
             put = [ xx++y | let xx = replicate x 'X' ++ if x < n then " " else "",
                             y <- _p xs (m-1) (r-x) (n-x-1) ]
             unput = if m + r > n
                     then []
                     else [ ' ':y | y <- _p xss m r (n-1) ]


-- 99. crossword puzzle

type Cord = (Int, Int)
type CrosswordR = [(Cord, Char)]

-- sloooooooooooow
solve99 :: String -> [CrosswordR]
solve99 = _solve . readCrossword
  where
    _solve (w, c) = filter (not . null) $ map isOK $ map (map (\(w, (c, _)) -> (c, w))) $
                 filter equalLength $ zipWith zip (repeat w) $ perm c (length w)
    perm _ 0 = [[]]
    perm xs n = [ x:y | x <- xs, y <- perm (delete x xs) (n-1) ]
    equalLength = all (\(w, (_, l)) -> length w == l)
    isOK xs = _isOK xs []
      where
        _isOK [] mp = mp
        _isOK ((cs, ws):xs) mp = case mergeMap (zip cs ws) mp of
                                    Nothing -> []
                                    Just m  -> _isOK xs m
        mergeMap [] ys = Just ys
        mergeMap (x@(a, b):xs) ys = case a `lookup` ys of
                                        Nothing -> mergeMap xs (x:ys)
                                        Just b' -> if b == b'
                                                   then mergeMap xs ys
                                                   else Nothing
    index = map (\(x, row) -> zipWith (\y c -> ((x, y), c)) [1..] row) . zip [1..]
    findWords = concatMap _f . index
      where
        _f = map (\g -> (map fst g, length g)) . filter (\g -> length g > 1 && (snd $ head g) == '.') . groupBy ((==) `on` snd)
    flipCord = map (\(cs, l) -> (map (\(x, y) -> (y, x)) cs, l))
    readCrossword s = let (w, _:a) = break ("" ==) $ lines s
                          c = findWords a ++ flipCord (findWords $ transpose a)
                      in  (w, c)


-- vim:ts=4:et
