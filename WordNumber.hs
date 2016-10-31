module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n 
  | n==0 = "zero"
  | n==1 = "one"
  | n==2 = "two"
  | n==3 = "three"
  | n==4 = "four"
  | n==5 = "five"
  | n==6 = "six"
  | n==7 = "seven"
  | n==8 = "eight"
  | n==9 = "nine"
  | True = error "not a digit"


digits :: Int -> [Int]
digits n = go n []
  where {
    go n l
      | n==0 = l
      | True = go (div n 10) ((mod n 10) : l)
}


wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits 





