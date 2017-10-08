import Data.Char(chr)

depunctuate :: String -> String
-- Remove punctuation from a given string
depunctuate cs = filter f cs
               where f c = if c `elem` ".,:" then False else True

makeString :: [Int] -> String
-- Turn a list of ints into a String
makeString xs = foldr (\x acc -> (chr x) : acc) [] xs
--makeString :: [Int] -> String
--makeString [] = []
--makeString (n : ns) = chr n : makeString ns

enpower :: [Int] -> Int
-- Multiply all the ints together in a list
enpower ns = foldl (\acc x -> acc * x) 1 ns

revAll ::[[a]] -> [a]
-- Reverse all the list in a list and add them together
revAll xs = foldr (\x acc -> reverse x ++ acc) [] xs

rev :: [a] -> [a]
-- Reverse all the elements in a given string
rev xs = foldl (\acc x -> x : acc) [] xs

dezip :: [(a,b)] -> ([a], [b])
-- Unpack a list of tuples into a tuple of two separate lists
dezip ps = foldr (\ x acc -> (fst x : fst acc, snd x : snd acc)) ([], []) ps

allSame :: [Int] -> Bool
-- Test if all the elements in a list are the same
allSame xs = and (zipWith (\x y -> x == y) xs (tail xs))

factorial :: Int -> Int
-- Calculate the fiven number's factorial
factorial n
  | n == 0 = 1
  | otherwise = n * factorial (n-1)

e :: Double
-- Caluculate the euler's number
e = e' 5
  where e' n
          | n == 0 = 1
          | otherwise = 1 / (fromIntegral (factorial n)) + e' (n-1)

squash :: (a -> a -> b) -> [a] -> [b]
-- Applies a given function to adjacent elements of a list
squash f xs = zipWith f xs (tail xs)

converge :: (a -> a -> Bool) -> [a] -> a
-- Pre : the list contains at least one element
-- Searches for convergence in a given list of values
converge f (x:xs) = foldl (\acc y -> if f y acc then acc else y) x xs

map' :: (a -> b) -> [a] -> [b]
-- Use foldr to implement map
map' f as = foldr (\a acc -> (f a) : acc) [] as

filter' :: (a -> Bool) -> [a] -> [a]
-- Use foldr to implement filter
filter' f as = foldr (\a acc -> if f a then a : acc else acc) [] as

repeatUntil :: (a -> Bool) -> (a -> a) -> a -> a
-- Apply a function to some intial value until the given predicate yields True
repeatUntil f g a
  | f (g a) = g a
  | otherwise = repeatUntil f g (g a)

any' :: (a -> Bool) -> [a] -> Bool
any' f as = or (map f as)

all' :: (a -> Bool) -> [a] -> Bool
all' f as = and (map f as)

