f :: Bool -> Int
f False = 0

fib :: Integral a => a -> a
fib 0 = 1
fib 1 = 1
fib n = (n-1) + (n-2)


recsum :: Int -> Int
recsum 1 = 1
recsum n = if (n<1) then error "number must be a positive integer" else n + recsum (n-1)

rectimes :: Int -> Int -> Int
rectimes m n
  | (m<0 || n<0) = error "need nonnegative integers"
  | (m==0 || n==0)  = 0
  | (m==1) = n
  | True = n + rectimes (m-1) n

data DividedResult = Result (Int, Int) | DividedByZero

instance Eq DividedResult where
  Result (x,y) == Result (x',y') = (x==x' && y==y')
  DividedByZero == DividedByZero = True
  _ == _ = False

instance Show DividedResult where
  show (Result (x,y)) = "Result (" ++ (show x) ++ ", " ++ (show y) ++ ")"
  show DividedByZero = "DividedByZero"

dividedBy :: Int -> Int -> DividedResult 
dividedBy dividend divisor = go dividend divisor 0
 where {go dividend divisor result
       | (divisor*dividend < 0) = let {final = go (abs dividend) (abs divisor) result} in (if (final == DividedByZero) then final else (let {Result (this, that) = final} in Result ((-1)*this, that)))
       | divisor == 0 = DividedByZero
       | dividend < divisor = Result (result, dividend)
       | True = go (dividend - divisor) divisor (result+1)}
	



