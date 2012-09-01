{-# LANGUAGE NoImplicitPrelude #-}
import Prelude ((.), (+), (-), (<), (>), (==), Bool (..), Int, Maybe (..), error, fst, not, otherwise, snd)

(!!) :: [a] -> Int -> a
(!!) l i = loop 0 l
  where loop _ []                 = error "index too large"
        loop p (x:xs) | p == i    = x
                      | otherwise = loop (p + 1) xs

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:[]) ys = x : ys
(++) (x:xs) ys = x : (xs ++ ys)

all :: (a -> Bool) -> [a] -> Bool
all f []     = True
all f (x:xs) | f x       = any f xs
             | otherwise = False

any :: (a -> Bool) -> [a] -> Bool
any f []     = False
any f (x:xs) | f x       = True
             | otherwise = any f xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = loop x
  where loop [] = concat xs
        loop (x':xs') = x' : loop xs'

drop :: Int -> [a] -> [a]
drop _ []       = []
drop n l@(x:xs) | n > 0     = drop (n - 1) xs
                | otherwise = l

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f l@(x:xs) | f x       = dropWhile f xs
                     | otherwise = l

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) | f x = x : traverse
                | otherwise = traverse
                where traverse = filter f xs

find :: (a -> Bool) -> [a] -> Maybe a
find f []     = Nothing
find f (x:xs) | f x       = Just x
              | otherwise = find f xs

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ z []     = z
foldl f z (x:xs) = foldl f (z `f` x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = x `f` foldr f z xs

head :: [a] -> a
head [] = error "empty list"
head (x:_) = x

init :: [a] -> [a]
init [] = error "empty list"
init (x:[]) = []
init (x:xs) = x : init xs

intercalate :: [a] -> [[a]] -> [a]
intercalate _ []          = []
intercalate xs' (x':xss') = unwind x' (loop xs' xss' [])
  where loop _ [] acc       = acc
        loop xs (x:xss) acc = unwind xs (unwind x (loop xs xss acc))
        unwind [] acc       = acc
        unwind (x:xs) acc   = x : unwind xs acc

intersperse :: a -> [a] -> [a]
intersperse _ []       = []
intersperse _ l@(x:[]) = l
intersperse e (x:xs)   = x : e : intersperse e xs

last :: [a] -> a
last [] = error "empty list"
last (x:[]) = x
last (_:xs) = last xs

-- Using an accumulator makes this almost 8 times faster than the naive approach
length :: [a] -> Int
length l = loop l 0
  where loop [] acc     = acc
        loop (_:xs) acc = loop xs (acc + 1)

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

null :: [a] -> Bool
null [] = True
null _  = False

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f l = (filter f l, filter (not . f) l)

reverse :: [a] -> [a]
reverse l = loop l []
  where loop [] acc     = acc
        loop (x:xs) acc = loop xs (x:acc)

tail :: [a] -> [a]
tail [] = error "empty list"
tail (_:xs) = xs

take :: Int -> [a] -> [a]
take _ []     = []
take n (x:xs) | n > 0     = x : take (n - 1) xs
              | otherwise = []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f l@(x:xs) | f x       = x : takeWhile f xs
                     | otherwise = []

-- transpose :: [[a]] -> [[a]]
-- transpose []   = []
-- transpose [[]] = []
-- transpose (x:xs) =
