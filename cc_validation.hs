--Turn a number into a list of single-digit numbers
toDigits :: (Integral a) => a -> [a]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

--Reverse a list
toDigitsRev :: (Integral a) => [a] -> [a]
toDigitsRev [] = []
toDigitsRev (x:xs) = (toDigitsRev xs) ++ [x]

--Double every other digit in a list starting from the right
doubleEveryOther :: (Integral a) => [a] -> [a]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther l@(x:y:ys)
	| even $ length l = (x*2):y:doubleEveryOther ys 
	| otherwise = x:(y*2):doubleEveryOther ys 

--Split digits >1 character in length into separate digits
--sumDigits :: (Num a) => [a] -> a
toDigits' :: (Integral a) => [a] -> [a]
toDigits' [] = []
toDigits' [x] = toDigits x
toDigits' (x:xs) = toDigits x ++ (toDigits' xs)

--Sum digits, using toDigits' to split all digits into 1 character
sumDigits :: (Integral a) => [a] -> a
sumDigits x = sum (toDigits' x)

--Validate that the number is a real credit card number
--validate :: a -> Bool
--validate x = num `mod` 10 == 0 where num = (sumDigits $ doubleEveryOther x)
validate :: (Integral a) => a -> Bool
validate x = (sumDigits $ doubleEveryOther $ toDigits x) `mod` 10 == 0