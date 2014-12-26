{-
prime [] = []
prime 0 = 0
prime x
	| (x `mod` n) == 0 && (n <= x) = x:[]
	| otherwise = (n+1) 
	where n = (n+1)
-}
--isdivis p = [x | x <- [1..p], p `mod` x == 0]
--prime [x] = [n | n <- [x,(x-1)..], map (`mod` x) n]

--prime (p:ps) = [x | x <- [1..p], p `mod` x == 0 ]

--prime [] = []
--prime (p:ps) = filter ([x | x <- [1..p], p `mod` x == 0 && ]) x : ps

--primes y [p] = filter (\x y -> y `mod` x == 0) p

--prime _ [] = []
--Actually makes factors...
--prime [] = []
--prime (p:ps) = [x | x <- [1..p], p `mod` x == 0] ++ prime ps

--s is starting value
--prime s (p:ps) = [x | x <- [(s+1)..p], p `mod` x == 0] ++ prime p ps

--primes p = [x | x <- [1..p], p `mod` x == 0]

--I lied, none of this works

{-- Works but starts from smallest and calculates up. We want to start from largest and go down
factors n = [x | x <- [1..n], n `mod` x == 0]

isprime 2 = True
isprime 3 = True
isprime n = if (even n || n `mod` 3 == 0) then False else all (/=0) $ map (n `mod`) [2..(floor (sqrt (fromIntegral n)))]

primefacs n = filter (isprime) $ factors n
-}

--Gives you the floored square of a number
sqrt' n = (floor (sqrt (fromIntegral n)))

--Calculate factors of n from largest factor to smallest
factors n = [x | x <- [(sqrt' n),((sqrt' n) - 1)..1], n `mod` x == 0]

--Determine if a number is prime. Should tweak more to cover 1-3 without pattern matching.

isprime 1 = False --1 isn't prime, who knew? Not me for a while
isprime 2 = True
isprime 3 = True
isprime n 
	| n <= 0 = error "Not Optimus" --Bad joke
	| (even n || n `mod` 3 == 0) = False --If number is even or divisible by 3 it's not prime.
	| otherwise = notElem 0 $ map (n `mod`) [(nsquare),(nsquare - 1)..4]
	where nsquare = sqrt' n
	{- 	Otherwise we divide the number by all possible numbers from sqrt(n) to 4 (4 to sqrt n might work better)
		I wonder if I can takeWhile on this list to avoid going through the whole map.
		Also wonder if I can re-use the "factors n" computation here since I'm essentially computing factors over and over again...
	-}

primefacs n = filter (isprime) $ factors n --Pretty sure this part is correct