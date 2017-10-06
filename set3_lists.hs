precedes::String->String->Bool
precedes (x:xs) (y:ys) = if (x <= y)
                         then True
                         else False
precedes [] (y:ys) = True
precedes (x:xs) [] = False
precedes [] [] = True

pos::Int->[Int]->Int
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
rev2 :: [Int]->[Int]
rev2 [] = []
rev2 xs = helper xs []
        where helper ys ac
	  | ys == [] = ac
	  | otherwise = helper ys ((head ys) : ac)

getSuffixSubStringList :: String -> [String]
getSuffixSubStringList xs = getSuffixSubStringList' xs []
                          where getSuffixSubStringList' ys ac = getSuffixSubStringList' (tail ys) (ys : [])

isSubstring :: String -> String -> Bool
isSubstring sub str = or [ sub == getPrefix n s | s <- strSuffixList]
                    where n = length sub
		          strSuffixList = getSuffixSubStringList str
			  getPrefix ln s = take ln s


transpose :: String -> String -> String -> String
transpose cs as (b:bs) = (cs !! pos b as) : transpose cs as (tail bs)  
                where transpose' cs as' (b':bs') ac
		  = (cs !! pos 'b as') : transpose' cs as'




                    



