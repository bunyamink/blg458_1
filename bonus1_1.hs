dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = mod ((d + t1 + k + t2 + t3) + 5 * j) 7
  where
    j :: Integer
    j = quot y 100
    k :: Integer
    k = mod y 100
    m' :: Integer
    m'
      | m <= 2 = m + 12
      | otherwise = m
    t1 :: Integer
    t1 = floor (fromIntegral (13 * (m' + 1)) / 5)
    t2 :: Integer
    t2 = floor (fromIntegral k / 4)
    t3 :: Integer
    t3 = floor (fromIntegral j / 4)
