-- sum of all natural numbers between 1..999 that are divisible by 3 or 5
--a = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
-- f = [x | x <- [1..1000]]
--fibonacci = [x+y | x <- [1..10], y <- [x..10]]
{-
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)
fibs n = fibs fibonacci!! n
-}

--fib [x] = [x+y | x <- head x, y <- snd x]
--fibonacci x = fibonacci (x-1) + fibonacci (x-2)
--fib x y = [fnum ++ (snum+fnum) | fnum <-x, snum <- head y]

--fib x y = [fib x]++[fib x+y]
--fib 0 = 0
--fib 1 = 1
--fib x y z = [xys = x:fib | xys <-x:x+y:xys  x<= z]
--fibs = 0:1:zipWith (+) fibs (tail fibs)
--fibs :: Integral [a] -> [a]
--fibs = [0:1:sum drop 2 fibs]
{-n = 0
last2 xs = drop (length xs - 2) xs
fibs = 0:1:sum last2 (take sizeOf fibs), sizeOf <- n
	where
	n = n+2-}

--last2 xs = drop (length xs - 2) xs
--fibs = xs@(x:ys) in 0:1:sum (last2 xs)
{-last2 [] = []
last2 [a] = [a]
last2 [a,b] = [a,b]
last2 xs = drop (length xs - 2) xs
fibs = 0:1:sum (last2 (takeWhile (>=0) fibs))-}

--fibs = (sum (drop 2 fibs))(1,0)

--fibs = (0:1):(sum )
--fibs x = (0:1)(sum take x fibs x+1)

--This works but is slow
{-fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)
fibs n = [fib x | x <- [1..n]]
-}

--fibo = map (+y) x | x <

--fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--fib = [x | x<-(0 : 1 : sum (drop 2 x))]

--fibs = 0 : 1 : [x | x <- (sum )]


--getmiddle :: [[a]] -> [a]
{-getmiddle [] = []
getmiddle x
	| even (length x) = (x!!n) ++ (x!!(n+1)) --middle two locations
	| otherwise = (x!!n) --middle location
	where n = ((length x) `div` 2)
-}
-- getmiddle :: [[a]] -> [a]

addone [] = []
addone (x:xs) = (x+1): addone xs

getmiddle [] = []
getmiddle x
	| even len = (x!!n) ++ (x!!(n+1)) --middle two locations
	| otherwise = (x!!n) --middle location
    where 
    	n = (len `div` 2)
        len = (length x)

lowORUP [] = []
lowORUP x
	| low == up = "Same number of capitals and lowercase"
	| low > up = "More lowercase than uppercase"
	| otherwise = "More uppercase than lowercase"
	where 
	low = length [c | c<-x, elem c ['a'..'z']]
	up = length [c | c<-x, elem c ['A'..'Z']]

 --Lets us index at 1
(!!!) _ 0 = error "!!! indexes starting from 1"
(!!!) x n = x !! (n-1)

kelem l n --Function pulls k'th elem of list starting at a 1 index
	| (n > length l) || (n < 1) = error "Number out of range"
	| otherwise = l !! (n - 1) --Subtract 1 so we index like haskell

--Get the length of a list
len' [] = []
len' (x:xs) = 1: len' xs
length' x = sum (len' x)

reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs)
	| length xs > 0 = (reverse' xs) : x
	| otherwise = []