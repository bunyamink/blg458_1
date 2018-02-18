import Debug.Trace

dayOfWeek :: Integer -> Integer -> Integer -> (Integer,Integer,Integer,Integer,Integer,Integer,Integer)
dayOfWeek y m d = traceShowId (j,k,m',t1,t2,t3,res)  
  where
    j :: Integer
    j = quot y 100
    k :: Integer
    k = mod y 100
    m' :: Integer
    m'
      | m >= 2 = m + 1
      | m == 1 = 14
      | m == 0 = 13
      | otherwise = m
    t1 :: Integer
    t1 = floor (fromIntegral (13 * (m' + 1)) / 5)
    t2 :: Integer
    t2 = floor (fromIntegral k / 4)
    t3 :: Integer
    t3 = floor (fromIntegral j / 4)
    res :: Integer
    res = mod ((d + t1 + k + t2 + t3) + 5 * j) 7