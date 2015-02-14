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

--Add a node to a tree. HELP RUNAWAY TYPES HELPPPPPPPPPPPPPPPP
-- addNode :: a -> Tree a -> Tree a
-- addNode a (Node n left@(Node lN lL lM lR) x right@(Node rN rL rM rR)) =
--   if odd n
--      then Node n+1 left a right
--      else Node n+1 right a left

--IF I BREAK THE SUMMING INTO A SEPARATE FUNCTION THEN I CAN RETURN THE TUPLE I WANT WITH LEFT AND RIGHT LENGTHS
-- treeLen :: (Eq a) => Tree a -> Integer
-- treeLen (Node _ l _ r) = left + right
--   where
--     left = if l == Leaf then 0 :: Integer else 1 + treeLen l
--     right = if r == Leaf then 0 :: Integer else 1 + treeLen r

-- addNode :: (Eq a) => a -> Tree a -> Tree a
-- addNode value tree@(Node n l v r)
--     | l == Leaf || r == Leaf = (Node n+1 Leaf value Leaf)
--     |
--     | otherwise =
--   where nl = treeLen l
--         nr = treeLen r

-- foldTree :: [a] -> Tree a
-- foldTree = foldr (\x acc@(Node n left value right) ->

--Produce balanced binary tree of a list using foldr.
-- foldTree :: [a] -> Tree a
-- foldTree [] = Leaf
-- -- foldTree = foldr (\x -> (Node (+1) foldTree) x (Node (+1) foldTree)) (Node 0)
-- foldTree xs = foldr (\x -> (Node node $ foldTree left) x (Node node $ foldTree right)) Leaf
--   where
--     split = splitHalf xs
--     left  = fst split
--     right = snd split
--     node = length left - 1

-- xor :: [Bool] -> Bool
-- xor x = odd $ foldr (\x -> if x==True then (+1) else (+0)) 0 x

xor' :: [Bool] -> Bool
xor' = foldr (\x acc -> (x || acc) && not (x && acc)) False

map' :: (a ->b) ->[a] ->[b]
map' f = foldr (\x acc -> f x : acc) []

-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr (\x acc -> ((x `f` acc) `f`)) base xs
--
f1 :: a -> a -> a
f1 x y = x
