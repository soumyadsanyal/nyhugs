__ :: (a -> b) -> [a] -> [b]
__ = map

(##) :: (Int -> Int) -> [Int] -> [Int]
f ## l = map f l


