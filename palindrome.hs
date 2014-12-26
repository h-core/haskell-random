--palindrome n = map (\o -> [x | x <- [10^(n-1)..(take n (repeat 9))]]) [] 

--Produce 2 lists of numbers, one from 100 to 999, one offset for 101..999
--Use show to print string representation
--Reverse the string and compare to original 
	--print where true
--Convert back to ints
--Get largest int
--Order sushi (not allowed until program is completed)

--Computes from 100*101 to 999*999
nums = [x * y | x <- [100..999], y <- [101..999]] 
--Move over list, turn into strings
numstrings [] = []
numstrings (x:xs) = [x | x <- show x] : numstrings xs
--Reverse strings
revs [] = []
revs (x:xs) = [x | x <- reverse x] : revs xs
--Compare two strings and print where equal (ie. where they're pals)
pals [] _ = []
pals _ [] = [] --We can't compare uneven lists so if either is empty we just return empty list
pals (x:xs) (y:ys)
	| x == y = x : pals xs ys
	|otherwise = pals xs ys
--Print our mofo'in pal-dromes
palindromes = pals (numstrings nums) (revs $ numstrings nums)

--Go back to ints (string to int)
stoi :: [String] -> [Int]
stoi = map read

--Biggest pal, iterate over list, find biggest number, print it
bigpal = maximum (stoi $ palindromes)