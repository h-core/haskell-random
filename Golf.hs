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
-- I was trying to do something a bit more dynamic than the hard 0..9 axis of the homework. This has led to a variety of issues...
-- not rendering right over 9 anyway, AND bunch of new lines cropping up where they shouldn't probably because i put them in the wrong spot.
uniquevalues :: (Num t, Enum t, Ord t) => [t] -> [(t, Int, t)]
uniquevalues vs = [(a,c,max')
  | a <- [min' .. max'],
  let c  = (length $ filter (==a) vs)]
    where max' = maximum vs
          min' = minimum vs

--Render the bar to be a string of '*' representing the count.
--Under each bar print the axis value
-- renderbar :: (Integer,Int,Int) -> String
renderbar :: (Int, Int, Int) -> String
renderbar (a,c,d) = replicate (d-c) ' ' ++ replicate c '*' ++ spacing a '=' ++ spacing (d-a) ' ' ++ show a ++  []
  where spacing x = replicate (length (show x))
-- histogram :: [Integer] -> String
-- Doesn't render right over 9...throwing in an extra \n, has something to do with renderbar since it only renders a single column at a time
histogram vs = foldl1 (++) $ map (++"\n") $ transpose $ map renderbar $ uniquevalues vs



