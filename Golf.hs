module Golf where

import Data.List

-- skips :: [a] -> [[a]]
-- skips [] = [[]]
--skips s = skips map tails (skips s)
--skips s = [a | a <- (s !! [0..5])]

localMaxima :: [Integer] ->[Integer]
localMaxima (a:b:c:cs)
  | b > a && b > c = b:localMaxima (b:c:cs)
  | otherwise = localMaxima (b:c:cs)
localMaxima _ = []

localMaxima' :: [Integer] ->[Integer]
localMaxima' = undefined

--data Bar = Bar Integer | Empty

--Identify all unique values in the integer array (with nub), and produce their counts.
--Make sure all missing values get an empty bar
uniquevalues :: [Integer] -> [(Integer,Int)]
uniquevalues n = [(a,c)
  | a <- [minimum n .. maximum n],
  let c  = (length $ filter (==a) n)]

--Render the bar to be a string of '*' representing the count.
--Under each bar print the axis value
renderbar :: (Integer,Int) -> String
renderbar (a,c) = show a ++ "|" ++ replicate c '*'

--Transpose all bars so they go from horizontal to vertical
--Print the bars

-- histogram :: [Integer] -> String
-- Horizontal instead of vertical at the moment. Also returning an IO () instead of a string obviously...
histogram vs = mapM_ putStrLn $ map renderbar $ uniquevalues vs


-- uniquevalues [] =
-- bars 0 = Empty
-- bars n = undefined

-- localMaxima [x] = []

-- localMaxima l@(x:y:zs) = [b | a <- l, b <- (y:zs), c <- zs, b>a, b>c]

-- localMaxima l@(x:y:zs) = [a | a <- (y:zs), b <- l, c <- zs, a>b, a>c]
--
-- localMaxima _ = []
-- localMaxima (x:y:z:zs) = [(filter ((x < y) && (y > z)) (y:z:zs))] ++ (localMaxima y:z:zs)

-- localMaxima :: [Integer] -> [Integer]
-- localMaxima l@(x:xs) = map (\y -> (>)y) xs
-- localMaxima = filter ((x < y) && (y > z))

-- localMaxima = foldr1 (\x -> (<) x && x (>))

-- histogram :: [Int] -> String

