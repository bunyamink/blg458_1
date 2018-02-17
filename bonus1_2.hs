dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = mod ((d + t1 + k + t2 + t3 + 5) * j) 7 
  where
    j :: Integer
    j = quot y 100
    k :: Integer
    k = mod y 100
    a :: Integer
    a = m + 12
    m' :: Integer
    m'
      | m <= 2 = a
      | otherwise = m
    t1 :: Integer
    t1 = floor (fromIntegral (13 * (m' + 1)) / 5)
    t2 :: Integer
    t2 = floor (fromIntegral k / 4)
    t3 :: Integer
    t3 = floor (fromIntegral j / 4)

sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start end
  where
    sundays' :: Integer -> Integer -> Integer
    sundays' y m
      | y > end = rest
      | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
        where
          nextY = y
          nextM = m
          rest = 0