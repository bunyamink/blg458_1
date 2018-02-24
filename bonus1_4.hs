leap :: Integer -> Bool
leap y
  | ((mod y 4) == 0 && (mod y 100 /= 0)) || mod y 400 == 0 = True
  | otherwise = False
  
dayInMonth :: Integer -> Integer -> Integer
dayInMonth m y = days
  where
  days :: Integer
  days
    | m == 2 = if leap y then 29 else 28
    | m == 4 || m == 6 || m == 9 || m == 11 = 30
    | otherwise = 31

sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays' 1 start
  where
    sundays' :: Integer -> Integer -> Integer
    sundays' m y
      | y > end = n
      | otherwise = if mod weekday 7 == 0 then n + 1 else n      
      where
        days = dayInMonth m y
        weekday = weekday + mod days 7
        nextY = if m == 11 then y + 1 else y
        nextM = mod (m + 1) 12
        n = if y > end then 1 else sundays' nextM nextY