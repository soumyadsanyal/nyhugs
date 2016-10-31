module Arith4 where
-- arith4.hs

-- id :: a -> a
-- id x = x
roundTrip :: (Show a, Read a) => a -> a 
roundTrip a = read .show $ a

roundTrip' :: (Show a, Read b) => a -> b 
roundTrip' x = (read . show $ x)


main = do
  print ( (roundTrip' 4)  :: Int)
  print (id 4)


