module WordNumber where

import Data.List (intersperse)

data SignedDigits = SignedDigits Char [Int]

instance Show SignedDigits where
  show (SignedDigits c l) = c : (show l)


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


properDigits :: Int -> SignedDigits
properDigits n = if n<0 then SignedDigits '-' (digits . abs $ n) else SignedDigits '+' (digits n)

digits :: Int -> [Int]
digits n = go n []
  where {
    go n l
      | n==0 = l
      | True = go (div n 10) ((mod n 10) : l)
}


wordNumber :: Int -> String
wordNumber n = (if sign == '-' then "negative " else "") ++ (concat . intersperse "-" . map digitToWord $ number)
  where {
  SignedDigits sign number = properDigits n
}





