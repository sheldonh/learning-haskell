import System.Environment

data RpnOp = RpnInt Int
           | RpnOp (Int -> Int -> Int)

main = do
  ops <- getArgs
  putStrLn (show (solve (interpret ops)))

interpret :: [String] -> [RpnOp]
interpret [] = []
interpret (x:xs) = toRpnOp x : interpret xs
  where toRpnOp "+" = RpnOp (+)
        toRpnOp "-" = RpnOp (-)
        toRpnOp "*" = RpnOp (*)
	toRpnOp "/" = RpnOp (div)
	toRpnOp xs' = RpnInt (read xs' :: Int)

solve :: [RpnOp] -> Int
solve ops = go ops []
  where go [] ((RpnInt i):_) = i
        go ((RpnInt i):xs) stack = go xs ((RpnInt i):stack)
        go ((RpnOp f):xs) ((RpnInt x):(RpnInt y):stack) = go xs ((RpnInt (f x y)):stack)
