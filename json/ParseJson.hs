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
parseInt all@(x:xs) | x == '-'  = (JsonInt (negate int), remaining)
                    | otherwise = (JsonInt int, remaining)
                      where (int, remaining) = parseInt' digits 0 1
                            digits           = if x == '-' then xs else all

parseInt' :: [Char] -> Int -> Int -> (Int, [Char])
parseInt' all@(x:xs) i m | isDigit x = parseInt' xs decimalValue (m * 10)
                         | otherwise = (i, all)
                           where decimalValue = i * m + digitToInt x

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

