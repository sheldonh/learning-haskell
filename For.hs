for :: a -> (a -> a) -> (a -> Bool) -> (a -> b) -> [b]
for i step while fn | while i   = (fn i) : (for (step i) step while fn)
                    | otherwise = []

-- for 0 succ (< 2) id
-- (id 0) : (for (succ 0) succ (< 2) id)
-- (id 0) : (for 1        succ (< 2) id)
-- (id 0) : (id 1) : (for (succ 1) succ (< 2) id)
-- (id 0) : (id 1) : (for 2        succ (< 2) id)
-- (id 0) : (id 1) : []
-- 0      : 1      : []
