import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl1' (*) . map (subtract 2) . filter (even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- fun2' :: Integer -> Integer
-- fun2' n = takeWhile

--Tree data structure
data Tree a = Leaf
             | Node Integer (Tree a) a (Tree a)
             deriving (Show, Eq)

--Split list in half
splitHalf :: [a] -> ([a],[a])
splitHalf x = splitAt (div (length x) 2) x

--Add a node to a tree

addNode :: a -> Tree a -> Tree a
addNode [] x = Leaf

--Produce balanced binary tree of a list using foldr.
foldTree :: [a] -> Tree a
foldTree [] = Leaf
-- foldTree = foldr (\x -> (Node (+1) foldTree) x (Node (+1) foldTree)) (Node 0)
foldTree xs = foldr (\x -> (Node node $ foldTree left) x (Node node $ foldTree right)) Leaf
  where
    split = splitHalf xs
    left  = fst split
    right = snd split
    node = length left - 1

xor :: [Bool] -> Bool
xor x = odd $ foldr (\x -> if x==True then (+1) else (+0)) 0 x



