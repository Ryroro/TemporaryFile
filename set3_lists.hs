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
                          where getSuffixSubStringList' ys ac = getSuffixSubStringList' (tail ys) (ys : [])

isSubstring :: String -> String -> Bool
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
splitUp [] = []
splitUp xs = l
           where l = splitUp' xs
                 splitUp' xs'
                   | fst t == "" = []
                   | otherwise   = fst t : splitUp' (removeWhitespace (snd t))
                                 where t = nextWord xs'




