{- LANGUAGE MultiWayIf #-}
data Op = Operator ([Double] -> [Double])
        | Operand  Double

calc :: Op -> [Double] -> [Double]
calc (Operator calcFunc) argStack = calcFunc argStack
calc (Operand value)     argStack = value : argStack

calcExpr :: [Op] -> [Double] -> [Double]
calcExpr (op : tail) argStack = calcExpr tail . calc op $ argStack
calcExpr [] argStack = argStack

main :: IO ()
main = print (calcExpr [(Operand 2.0), (Operand 2.0), (Operator (\(sncd : fst : tail) -> sncd + fst : tail))] [])
