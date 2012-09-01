import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)

data JsonValue = JsonNull
               | JsonTrue
               | JsonInt Int
               | JsonBool Bool
               | JsonString [Char]
               | JsonList [JsonValue]
                 deriving (Eq, Show)

parseJsonValue :: [Char] -> (JsonValue, [Char])
parseJsonValue all@(x:xs) | isPrefixOf "null" all  = (JsonNull, drop 4 all)
                          | isPrefixOf "true" all  = (JsonBool True, drop 4 all)
                          | isPrefixOf "false" all = (JsonBool False, drop 5 all)
                          | isDigit x              = parseInt all
                          | x == '-'               = parseInt all
                          | x == '['               = parseList all
                          | x == '"'               = parseString all

parseInt :: [Char] -> (JsonValue, [Char])
parseInt ('-':xs) = let parsed = parseInt' xs 0 1 in (JsonInt (negate (fst parsed)), snd parsed)
parseInt xs       = let parsed = parseInt' xs 0 1 in (JsonInt (fst parsed), snd parsed)

parseInt' :: [Char] -> Int -> Int -> (Int, [Char])
parseInt' all@(x:xs) i m | isDigit x = let parsed = parseInt' xs (i * m + (digitToInt x)) (m * 10)
                                       in (fst parsed, snd parsed)
                         | otherwise = (i, all)

parseList :: [Char] -> (JsonValue, [Char])
parseList xs = let parsed = parseList' xs in (JsonList (fst parsed), snd parsed)

parseList' :: [Char] -> ([JsonValue], [Char])
parseList' (x:xs)
  | x `elem` "[," = let item = parseJsonValue (stripSpace xs)
                        rest = parseList' (stripSpace (snd item))
                        in (fst item : fst rest, snd rest)
parseList' (']':xs) = ([], xs)

parseString :: [Char] -> (JsonValue, [Char])
parseString ('"':xs) = let parsed = parseString' xs in (JsonString (fst parsed), snd parsed)

parseString' :: [Char] -> ([Char], [Char])
parseString' ('\\':x:xs) = let s = parseString' xs in ('\\' : x : (fst s), snd s)
parseString' ('"':xs) = ([], xs)
parseString' (x:xs) = let s = parseString' xs in (x : (fst s), snd s)

stripSpace :: [Char] -> [Char]
stripSpace (' ':xs) = stripSpace xs
stripSpace xs       = xs

