module HW5 where

listLength :: (Ord a) => [a] -> Integer
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

nthElem :: (Ord a) => [a] -> Integer -> a
nthElem (x:xs) a | a == 0	= x
		 | otherwise	= nthElem xs (a - 1)
nthElem [] a =	error ("List too short by " ++ show (a + 1) ++ " elements.")

removeFirst :: (Eq a) => [a] -> a -> [a]
removeFirst (x:xs) s | x == s	= xs
		     | otherwise= x:(removeFirst xs s)
removeFirst [] _  = []

--problem--
subst :: (Eq a) => a -> a -> [a] -> [a]
subst n o (x:xs) | x == o	= n:subst n o xs
		 | otherwise	= x:subst n o xs
subst _ _ [] = []

listSum :: [Integer] -> Integer
listSum (x:xs) = x + listSum xs
listSum [] = 0

duple :: (Num a, Eq a, Ord b) => a -> b -> [b]
duple c e | c == 0	= []
	  | otherwise	= e:duple (c-1) e

--Hint: use a helper function
sortPred :: (Eq a) => (a -> a -> Bool) -> [a] -> [a]
sortPred p (x:xs) = insert p x (sortPred p xs)
sortPred _ [] = [] 

insert :: (Eq a) => (a -> a -> Bool) -> a -> [a] -> [a]
insert p e f | (f == []) || (p e (head f))	= e:f
	     | otherwise			= (head f):insert p e (tail f)
