-- Naive recursive approach leads to exponential time
fibonacci :: Int -> Int
fibonacci n | n == 0    = 0
            | n == 1    = 1
            | otherwise = fn' + fn''
              where fn'' = fibonacci (n - 2)
                    fn'  = fibonacci (n - 1)

-- fibonacci 6
-- ((f 5) + (f 4))
-- (((f 4) + (f 3)) + ((f 3) + (f 2)))
-- ((((f 3) + (f 2)) + ((f 2) + (f 1))) + (((f 2) + (f 1)) + ((f 1) + (f 0))))
-- (((((f 2) + (f 1)) + ((f 1) + (f 0))) + (((f 1) + (f 0)) + 1)) + ((((f 1) + (f 0)) + 1) + (1 + 0)))
-- ((((((f 1) + (f 0)) + 1) + (1 + 0)) + ((1 + 0) + 1)) + (((1 + 0) + 1) + (1 + 0)))
-- (((((1 + 0) + 1) + 1) + (1 + 1)) + ((1 + 1) + 1))
-- ((((1 + 1) + 1) + 2) + (2 + 1))
-- (((2 + 1) + 2) + 3)
-- ((3 + 2) + 3)
-- (5 + 3)
-- 8

-- Memoized iterative approach leads to linear time
fibonacci' :: Int -> Int
fibonacci' n | n == 0    = 0
             | otherwise = go 0 1 1
  where go a b i | i == n    = b
                 | otherwise = go b (a + b) (i + 1)

-- fibonacci' 6
-- go 0 1 1
-- go 1 1 2
-- go 1 2 3
-- go 2 3 4
-- go 3 5 5
-- go 5 8 6
-- 8
