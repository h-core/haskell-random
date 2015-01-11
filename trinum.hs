--trinum :: Int -> [Int]
--Start with a seed value, sum 1..seed, then 1..seed+1, etc.
--This version of trinum sums too many numbers
--trinum x = (sum [1..x]): trinum (x+1)

--scanl1 is like foldl1 except it's cumulative over the entire list so in scanl1 (+) [x,y,z]
--we get [x,x+y,x+y+z]
trinum :: [Integer]
trinum = scanl1 (+) [1..]

--Take target dnums (number of divisors), and a list, return values >= dnums.
--divisors dnums (x:xs) = [a | a <- [1..x], x `mod` a == 0]

--Our infinite list --Why is this here? trinum worked differently before and then I didn't feel like changing it.
inflist :: [Integer]
inflist = trinum

--Get the factors of a number. x/2 so that our list is limited to half the size of the number.
--We do this because half of any natural number will be the largest factor of a number
factors :: Integer -> [Integer]
factors x
	| even x = [y | y<-[2..(x `div` 2)], x `mod` y == 0 ]
	| otherwise = [y | y<-[1,3..(x `div` 2)], x `mod` y == 0 ]

--Check if length of an array is larger than a target value (t)
arrlen :: Int -> [Integer] -> Bool
arrlen t (x) = (length x) > t

--Filter faclist with a target (t) for arrlen so we get every number that has at least t factors
facGTt :: Int -> [Integer]
facGTt t = filter (arrlen t . factors) inflist

--List of prime numbers DOESN'T WORK
--primes = [x | x <-[2..], filter ((0 ==) x `mod`) [2..x]]

{-
<jesyspa> Factoring is hard.
<daishi> i suppose i could start at [2..] lol
<jesyspa> Do you really need all factors, rather than prime factors?
<daishi> i see
<daishi> yes
<jesyspa> Are you sure it isn't faster to get the prime factors and then count the non-prime factors from those?
<daishi> if i took the prime factors of a number i could then factor non-prime factors from the result to get all the factors of the number?
<jesyspa> Well...
<jesyspa> Let me just take an example.
<jesyspa> > 2 * 2 * 2 * 3 * 3 * 7
<lambdabot>  504
<jesyspa> You can take 2 zero through four times, take 3 zero through two times, and take seven zero or one time.
* southp has quit (Ping timeout: 264 seconds)
<jesyspa> Err, zero through three times for 2*
<jesyspa> So I think there should be 4 * 3 * 2 factors (minus two if you don't count 1 and the number itself)
<jesyspa> (I think you do count 1 but don't count the number itself.
<jesyspa> )
<jesyspa> > 4 * 3 * 2 - 1
<lambdabot>  23
* briennetheblue (~brienneth@host165-120-29-26.range165-120.btcentralplus.com) has joined
<jesyspa> > length [y | y<-[1..(504`div`2)], 504 `mod` y == 0]
<lambdabot>  23
<jesyspa> Might just be coincidence, but I don't think it is.
<jesyspa> And prime factors are still hard, but maybe if you reuse a list of primes it won't be quite that hard.
-}
