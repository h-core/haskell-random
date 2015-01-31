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

--Identify all unique values in the integer array (with nub), and produce their counts.
--Make sure all missing values get an empty bar
-- uniquevalues :: [Integer] -> [(Integer,Int,Int)]
uniquevalues :: (Enum t, Ord t) => [t] -> [(t, Int, t)]
uniquevalues n = [(a,c,max')
  | a <- [min' .. max'],
  let c  = (length $ filter (==a) n)]
    where max' = maximum n
          min' = minimum n

--Render the bar to be a string of '*' representing the count.
--Under each bar print the axis value
-- renderbar :: (Integer,Int,Int) -> String
renderbar :: Show a => (a, Int, Int) -> [Char]
renderbar (a,c,d) = replicate (d-c) ' ' ++ replicate c '*' ++ "=" ++ show a

-- histogram :: [Integer] -> String
-- Doesn't render right over 9?
-- Also returning an IO () instead of a string obviously...
histogram vs = mapM_ putStrLn $ transpose $ map renderbar $ uniquevalues vs



