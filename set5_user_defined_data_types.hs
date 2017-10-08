data Shape = Triangle Float Float Float
           | Square Float
           | Circle Float
           deriving (Show)

area :: Shape -> Float
area (Triangle a b c) = sqrt (s*(s-a)*(s-b)*(s-c))
                    where s = (a + b + c) / 2

area (Square a) = a * a

area (Circle r) = pi * r * r




