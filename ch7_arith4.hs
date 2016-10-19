module Arith4 where
-- arith4.hs

-- id :: a -> a
-- id x = x
roundTrip :: (Show a, Read a) => a -> a 
roundTrip a = read .show $ a


main = do
  print (roundTrip 4) 
  print (id 4)


