import Data.Char

addDigit::Int->Int->Int
addDigit a b = a*10 + b

celcius_to_fahrenheit::Double->Double
celcius_to_fahrenheit c = c * 9 / 5 + 32

type Vertex = (Float, Float)

distance:: Vertex -> Vertex -> Float
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

triangleArea::Vertex->Vertex->Vertex->Float
triangleArea (x1, y1) (x2, y2) (x3, y3) = sqrt (s*(s-a)*(s-b)*(s-c))
    where a = distance (x1, y1) (x2, y2)
          b = distance (x1, y1) (x3, y3)
          c = distance (x2, y2) (x3, y3)
          s = (a+b+c)/2

--isPrime::Int->Bool
--isPrime x = if x > 1 && [y | y<-[2..(sqrt x)], x `mod` y /= 0] == []
--            then True
--            else False

fact::Int->Int
fact 0 = 1
fact x = x * fact (x-1)

remainder::Int->Int->Int
remainder a b
    | a < b = a
    | a == b = 0
    | otherwise = remainder (a-b) b

quotient::Int->Int->Int
quotient a b
    | a < b = 0
    | a == b = 1
    | otherwise = 1 + quotient (a-b) b

binary::Int->[Char]
binary x
    | x < 2 = [chr (ord '0' + x)]
    | otherwise = binary ((x - x `mod` 2) `div` 2) ++ [chr (ord '0' + x `mod` 2)]
   
add::Int->Int->Int
add a b
    | b == 0 = a
    | otherwise = 1 + add a (pred b)

larger::Int->Int->Int
larger a b
    | a == 0 = b
    | b == 0 = a
    | otherwise = 1 + larger (pred a) (pred b)

chop::Int->(Int, Int)
chop a
    | 
