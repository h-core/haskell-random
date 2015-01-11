type Peg = String
type Move = (Peg, Peg)

--x is total number of discs
--Total moves for a 3 peg setup is (2^x)-1
--hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
{-hanoi x a b c = hanoi' ((2^x)-1) a b c

hanoi' :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi' 0 _ _ _ = []
hanoi' 1 _ _ _ = []
hanoi' n source by dest = [(source, dest)] ++ (hanoi' (n-1) source dest by)
-}

--Solves tower of hanoi. I don't understand it 100% but ehh...
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source dest store
	| n == 1 = [(source, dest)]
	| otherwise = hanoi (n-1) source store dest ++ [(source, dest)] ++ hanoi (n-1) store dest source