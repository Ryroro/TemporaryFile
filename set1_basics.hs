get_penultimate::Int->Int
get_penultimate x = div (mod x 100 - mod x 10) 10

get_time::Int->(Int, Int, Int)
get_time x = (x `div` 3600, (s `mod` 3600 - s `mod` 60) `div` 60, s `mod` 60)
