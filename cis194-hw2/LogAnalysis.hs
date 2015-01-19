--{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

--Take a string and parse it into a LogMessage. Doesn't catch malformed strings that 
--happen to start with an I/W/E
parseMessage :: String -> LogMessage
parseMessage s@(x:xs)
	| x == 'I' = LogMessage Info timeStampWI s
	| x == 'W' = LogMessage Warning timeStampWI s
	| x == 'E' = LogMessage (Error errorLevel) timeStampE s
	| otherwise = Unknown s
	where 
		sWords = words s
		errorLevel = read (sWords !! 1) :: Int
		timeStampWI = errorLevel
		timeStampE = read (sWords !! 2) :: Int

--Map parseMessage over a set of strings.
parse :: String -> [LogMessage]
parse x = map (parseMessage) $ lines x 

--Make a tree of messages, left < node, right > node.
--I don't know what I'm doing
insert :: LogMessage -> MessageTree -> MessageTree
insert x Leaf = Node Leaf x Leaf
insert x (Node Leaf _ right) = Node Leaf x right 
insert x (Node left _ Leaf) = Node left x Leaf
insert x (Node Leaf y Leaf) = Node Leaf y (Node Leaf x Leaf)
insert (Unknown _) n = n
insert lM@(LogMessage mT tS s) (Node left nLm@(LogMessage _ x _) right)
	| tS < x = Node (insert lM left) nLm right
	| tS > x = Node left nLm (insert lM right)
	| tS == x = Node left lM (Node Leaf nLm right)
