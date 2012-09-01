reduceFraction :: Integral a => (a, a) -> (a, a)
reduceFraction (a, 0) = (a, 0)
reduceFraction (a, b) = let d = gcd' a b in (div a d, div b d)

-- Prelude has a gcd function, but since reducing fractions is all about gcd,
-- that would have been cheating.

gcd' :: Integral a => a -> a -> a
gcd' 0 0 = undefined
gcd' a b = go a b start
  where go a b i | mod a i == 0 &&
                   mod b i == 0 = i
                 | otherwise    = go a b (i - 1)
        start = max 1 (min (abs a) (abs b))
