--Pythagorean triplet a < b < c, a^2 + b^2 = c^2

--pytrip c = [(a*a) + (b*b) | a <- [1..c], b <- [(a+1)..c], (a*a)+(b*b) == c]
{-
pytrip c
	| (a<b) && (b<c)
	| (a*a)
-}

square x = x*x

--pytrip c = [a:b:c | a <- [1..sqrt(c)], b <- [(a+1)..sqrt(c)], (a < b), (b < c), (sqrt(a) + sqrt(b) == c)]

--pytrip :: Int -> [Int] -> [Int]
--pytrip c (x:xs) = (filter (==c) $ map (product x) $ take 2 (x:xs)): pytrip c xs

--Generate every permutation of x*y from x to y (ex. 1*1 -> 999*999)
--Not really for solving the problem I wanted to.
--Also need to figure out how to do this without forcing the starting (x) value to be 1
--after iterating over y
squareTups x y 
	| x < y = (a_sq,b_sq,c_sq):squareTups (x+1) y
	| (x==1)&&(y==1) = []
	| otherwise = squareTups 1 (y-1)
	where 
		a_sq = x^2
		b_sq = y^2
		c_sq = a_sq + b_sq

--Store our tups in a convenient variable
tups = squareTups 1 999

--Take third of 3-set tuple
thrd (_,_,x) = x

--Look for our target c_sq
pytrip c_sq = filter ((==c_sq).thrd) tups

--Take third from a 6-set tuple. Yes I know I'm probably doing it wrong at this point.
thrd' (_,_,_,_,_,x) = x

--Triples where I once again realize I don't know what I'm doing
{-
triplets a b c ceiling
	| (a+b+c) == ceiling = 
	| b >= c = triplets a b (c+1)
	| a >= b = triplets a (b+1) c
	| a < b = triplets
	| otherwise = triplets a b c
-}

--triplets ceiling = [(a,b,c) | a <- [1..(ceiling - 2)], b <-[]]

--All credit to qulj0t3 for explaining to me that list comprehension works like a nested loop.
--t is the target value (1000 in our case)
pytrips t = [(a,b,c) | c <- [1..t], b <- [1..c-1], a <- [1..b-1] a+b+c == t, (a^2 + b^2) == c^2]


