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
sundays2 start end