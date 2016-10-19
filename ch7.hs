f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f)) 
f x y = ((this, that), (here, there)) where
  (this, _, here) = x
  (that, _, there) = y


functionC :: (Ord a) => a -> a -> a
functionC x y = case (x>y) of 
  True -> x
  False -> y

ifEvenAdd2 n = case even n of 
  True -> (n+2) 
  False -> n

nums x = case compare x 0 of 
  LT -> -1 
  GT -> 1
  EQ -> 0

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10 

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1 

oneIsTwo :: (Num a) => a -> a
oneIsTwo = (flip dodgy) 2

tensDigit :: Integral a => a -> a 
tensDigit = snd . f . fst . f
  where f = flip divMod 10

hundredsDigit :: Integral a => a -> a 
hundredsDigit = snd . g . f
  where f = flip divMod 10
        g = f . fst

-- generalize the two exercises above
--
nthDigit :: Integral a => a -> Int -> a 
nthDigit = (.) ((.) ((.) snd) (!!)) h
  where f = flip divMod 10
        g = f . fst
        h = (iterate g) . f

foldBool :: a -> a -> Bool -> a
foldBool x y c = case c of
  True -> x
  False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y c 
  | c = x
  | True = y

g :: (a -> b) -> (a, c) -> (b, c)
g = \f ->
      \t ->
        (f . fst $ t, id . snd $ t) 








