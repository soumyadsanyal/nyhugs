
ns :: Char -> Bool
ns = (/= ' ')

ws :: Char -> Bool
ws = (== ' ')

lint :: String -> String
lint = dropWhile ws

myWords :: String -> [String]
myWords s
  | t == "" = []
  | True = (takeWhile ns t) : (myWords (lint $ dropWhile ns t))
  where {
    t = lint s
}

--f "a b c"
--"a" : f "b c"
--"a" : "b" : f "c"
--"a" : "b" : "c" : f ""
--"a" : "b" : "c" : []
--["a", "b", "c"]
--
