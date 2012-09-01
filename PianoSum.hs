-- First arrange x and y for the smallest number of iterations to
-- be performed by pianoSum'
--
pianoSum :: Int -> Int -> Int
pianoSum x y = if abs y > abs x
               then pianoSum' y x
               else pianoSum' x y

pianoSum' :: Int -> Int -> Int
pianoSum' x y | y > 0     = pianoSum' (x + 1) (y - 1)
              | y < 0     = pianoSum' (x - 1) (y + 1)
              | otherwise = x

