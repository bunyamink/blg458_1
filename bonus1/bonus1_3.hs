dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = mod ((d + t1 + k + t2 + t3) + 5 * j) 7  
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

sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1 0
  where
    sundays' :: Integer -> Integer -> Integer -> Integer
    sundays' y m acc
      | y > end = acc
      | otherwise = if dayOfWeek y m 1 == 1 then sundays' nextY nextM (acc + 1) else sundays' nextY nextM acc
        where
          nextY = if m == 11 then y + 1 else y
          nextM = mod (m + 1) 12