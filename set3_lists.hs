


import Data.Char(isSpace)

precedes :: String -> String -> Bool
precedes (x:xs) (y:ys) = if (x <= y)
                         then True
                         else False
precedes [] (y:ys) = True
precedes (x:xs) [] = False
precedes [] [] = True

pos::Eq a =>a->[a]->Int
-- Pre : the element must be in the list
-- Return the position of a given element in a list
pos a (x:xs)
    | x == a    = 0
    | otherwise = 1 + pos a xs

twoSame :: [Int]->Bool
twoSame []     = False
twoSame (x:xs) = x `elem` xs || twoSame xs


rev1 :: [Int]->[Int]
rev1 []     = []
rev1 (x:xs) = rev1 xs ++ [x]

-- More efficient one, I'm not very happy with this one
rev2 :: [Int] -> [Int]
rev2 [] = []
rev2 xs = rev2' xs []
        where rev2' ys ac
                | ys == []  = ac
                | otherwise = rev2' ys ((head ys) : ac)

getSuffixSubStringList :: String -> [String]
getSuffixSubStringList xs = getSuffixSubStringList' xs []
                          where getSuffixSubStringList' [] _ = []
                                getSuffixSubStringList' ys ac = ys : getSuffixSubStringList' (tail ys) (ys : [])

isSubstring :: String -> String -> Bool
-- Return whether or not the given string is a substring of the second
isSubstring sub str = or [ sub == getPrefix n s | s <- strSuffixList]
                    where n = length sub
                          strSuffixList = getSuffixSubStringList str
                          getPrefix ln s = take ln s


transpose :: String -> String -> String -> String
-- Reorder the characters in the first string based on the second
--   and the third string
transpose cs as bs
  | bs == [] = []
  | otherwise = (cs !! (pos (head bs) as)) : transpose cs as (tail bs)

removeWhitespace :: String -> String
-- Reomve whitespaces from a string
-- Pre : the first character is not a white space
removeWhitespace [] = []
removeWhitespace (x:xs)
  | isSpace x = removeWhitespace xs
  | otherwise = (x:xs)

nextWord :: String -> (String, String)
-- Pre : the first character is not a white space
-- Return a tuple of the first word and the remaining character
nextWord xs = nextWord' xs []
            where nextWord' [] ac = (ac,[])
                  nextWord' (x':xs') ac
                    | isSpace x' = (ac, (x':xs'))
                    | otherwise = nextWord' xs' (ac++[x'])

splitUp :: String -> [String]
-- Return a list of words contained in a given String
splitUp [] = []
splitUp xs = splitUp' (removeHeadWhitespace xs)
           where removeHeadWhitespace (x:xs)
                   | isSpace x = removeHeadWhitespace xs
                   | otherwise = (x:xs)
                 splitUp' xs'
                   | fst t == "" = []
                   | otherwise   = fst t : splitUp' (removeWhitespace (snd t))
                                 where t = nextWord xs'



getNumbers :: Int -> Int -> [Int]
getNumbers a b
-- Pre : b must be smaller or equal to a 
-- Return a list of consecutive ints ranging from a to b
  | a == b = [a]
  | otherwise = b : getNumbers a (b+1)

isPrime :: Int -> Bool
-- Test whether or not given int is a prime
isPrime a
  | a == 2 = True
  | otherwise = not (or [a `mod` x == 0 | x <- getNumbers (a-1) 2])

nextPrime :: Int -> Int
-- Return next prime that is greater than the given prime
nextPrime a
  | isPrime (a+1) = a+1
  | otherwise = nextPrime (a+1)

primeFactors :: Int -> [Int]
-- Return a list of prime factors for a given integer
primeFactors a = primeFactors' a 2
               where primeFactors' a b
                       | a == 1 = []
                       | a `mod` b == 0 = b : primeFactors' (a `div` b) b
                       | otherwise = primeFactors' a (nextPrime b)

hcf :: Int -> Int -> Int
-- Return the highest common factor of two ints
hcf a b = hcf' aList bList
        where aList = primeFactors a
              bList = primeFactors b
              hcf' (a:as) (b:bs)
                | a == b    = a * hcf' as bs
                | a < b     = hcf' as (b:bs)
                | a > b     = hcf' (a:as) bs
              hcf' _ _ = 1

findAll :: Int -> [(Int, Int)] -> [Int]
-- Find all the bindings for a given int in a list of pairs of ints
findAll x t = [y | (x', y) <- t, x == x']

--isSubstring2 :: String -> String -> Bool
-- Return whether or not the given string is a substring of a second list
-- Note : the implementation uses list comprehension and built-in functions
-- isSubstring2 

remove1 :: Eq a => a -> [(a, b)] -> [(a, b)]
-- Remove all the bindings for a given int in a list of pairs of ints
remove1 x [] = []
remove1 x (y:ys)
  | x == fst y = remove1 x ys
  | otherwise = y : remove1 x ys

remove2 :: Eq a => a -> [(a, b)] -> [(a, b)]
remove2 x t = [ y | y <- t, x /= fst y]

remove3 :: Eq a => a -> [(a, b)] -> [(a, b)]
remove3 x t = filter ((/=x) . fst) t

allSplits :: [a] -> [([a], [a])]
-- Returns the result of splitting a list at all possible points
allSplits [] = []
allSplits x = allSplits' x 1
              where l = length x
                    allSplits' x' n
                      | n == l = []
                      | otherwise = splitAt n x' : allSplits' x' (n+1)

findnodes :: Int -> [(Int, Int)] -> [(Int, Int)]
findnodes a nodes = [node | node <- nodes, fst node == x]

routes1 :: Int -> Int -> [(Int, Int)] -> [[Int]]
-- Given a graph defined by nodes and a specific node to another
--   return a specific route that connects the node
-- Note : this is the situation for acyclic graph
routes1 a b nodes = 
                  where l = 
                        routes1' :: Int -> Int -> (Int, Int) -> [[Int]]
                        routes1' x y node route
                          | snd node == y = [route ++ [x,y]]
                          | findnodes (snd node) nodes /= [] = [routes1' (snd node) y n (route ++ [x,y]) | n <- findnodes (snd node) nodes]
                          | otherwise = [[]]

                                                                                                   


