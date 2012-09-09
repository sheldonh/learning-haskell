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
solve ops = head $ foldl calculator [] ops

calculator :: [Int] -> RpnOp -> [Int]
calculator stack    (RpnInt i) = i : stack
calculator (x:y:xs) (RpnOp fn) = (fn y x) : xs
