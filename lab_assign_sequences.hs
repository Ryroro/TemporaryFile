arithmeticSeq::Double->Double->Int->Double
arithmeticSeq a d n = a + fromIntegral n * d

geometricSeq::Double->Double->Int->Double
geometricSeq a r n = a * r^n

arithmeticSeries::Double->Double->Int->Double
arithmeticSeries a d n = (n' + 1)*(a + d * n' / 2)
                       where n' = fromIntegral n

geometricSeries::Double->Double->Int->Double
geometricSeries a r n
  | r == 1    = a * (fromIntegral n + 1)
  | otherwise = a * (1 - r^(n+1))/(1-r)
