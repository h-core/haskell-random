import Data.List.Split

nums = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
--numl ns = stoi ns

--Split string at each character (already done by Haskell)
--Start from first index position (!! 0) to 13th char (!! 12)
	--Produce product
	--Check if current product > previous product
			--If so then keep current, otherwise use previous
	--Increase index and produce current product again
	--Go until 13 index positions from end
	--Return product
--prodNums :: Int -> [String] -> Int -> Int -> [String]
{-
prodNums _ [] = []
prodNums pp (n:ns)
	| length (n:ns) < 13 = prodNums pp []
	| cp > pp = prodNums cp (ns)
	| otherwise = prodNums pp (ns)
	where 
		cp = map (*) numl
-}
--List of strings to ints

{-
stringTake _ [] = []
stringTake z (x:xs) = take z (x:xs) : (stringTake z xs)
-}

--foldNum :: [[Char]] -> [Int]
--foldNum (x:xs) = [read x :: Int]

--multInList [[]] = [[]]
--multInList ((x:xs):ys) =  [x * xs | x -> (read x :: Int)]

--create sequences from a list, take the first x, lob off the head, repeat.
numTake _ [] = []
numTake z (x:xs) = take z (x:xs) : (numTake z xs)

--what we wanna do for euler
pnums = numTake 13 nums

--Turn a string into a list of ints
itoi n = map (\x -> read [x] :: Int) (show n)

--map product over map result of itoi over stoi pnums. also acknowledgement
--of my poor ability to pick variable names, or explain things
prodnums = map product $ (map itoi $ stoi pnums)

--Get the maximum of prodnums
mprod = maximum prodnums

--List string to Ints
stoi :: [String] -> [Int]
stoi = map read