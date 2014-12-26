--Take infinite list of numbers
--Check modulus of each number 2..y
--Stop and print number when all modulus of 2..y for a given x is 0

mult n = [x | x <- [1..], all (==0) $ map (x `mod`) [2..n]]

--Squaresssss
squares = [x*x | x <- [1..100]]
