----------------------------------------------------
-- Help Function that is not in the original document
----------------------------------------------------
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
----------------------------------------------------
-- Help Function ends
----------------------------------------------------

gcd' :: (Int, Int) -> Int
-- Return the greatest common divisor of two given integers
gcd' (m, n)
  | n == 0    = m
  | otherwise = gcd' (n, (m `mod` n))

phi :: Int -> Int
-- Return the number of integers (from 1 to m) that are relatively prime to m
phi m = count p
      where p = [ x | x <- [1..m], gcd'(x, m) == 1]
            count xs =  foldr (\x acc -> acc + 1) 0 xs

modPow :: Int -> Int -> Int -> Int
-- Given a, k, m, return a^k mod m
modPow a k m
  | even k = (a^2 `mod` m)^(efind k) `mod` m
  | odd k  = (a*((a^2 `mod` m)^(ofind k) `mod` m)) `mod` m
           where efind k' = k' `div` 2
                 ofind k' = (k'- 1) `div` 2

extendedGCD :: Int -> Int -> ((Int, Int), Int)
-- Given two non negative integers, return a pair of Bezout coefficients
--   and their greatest common divisor
extendedGCD a b = (extendedGCD' a b, gcd' (a, b))
                where extendedGCD' a b
                        | b == 0 = (1, 0)
                        | b > 0 = extendedGCD(a, a `mod` b)

smallestCoPrimeOf :: Int -> Int
-- Given non zero integer a, returns the smallest integer b > 1
--  that is coprime with a
-- Note : Applies the helper function on top of the file
smallestCoPrimeOf a = smallestCoPrimeOf' a 2
                    where smallestCoPrimeOf' a b
                            | gcd' (a, b) == 1 = b
                            | otherwise        = smallestCoPrimeOf' a (nextPrime b)

genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
-- Given two distinct prime numbers, runs the RSA key generator and returns key pairs
genKeys p q = ((e, n), (d, n))
            where n = p * q
                  e = smallestCoPrime ((p-1)*(q-1))
                  d = ((p-1) `mod` (q-1)) `div` e

rsaEncrypt :: Int -> (Int, Int) -> Int
-- Takes a plain text x and a public key (e, N) and returns the ciphertext
rsaEncrypt x (e, n) = x^e `mod` n

rsaDecrypt :: Int -> (Int, Int) -> Int
-- Takes a ciphertext c and a private key (d, N) and returns the plaintext
rsaDecrypt x (d, n) = c^d `mod` n






