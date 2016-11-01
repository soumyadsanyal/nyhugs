
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

linter :: Char -> String -> String
linter breakpoint = dropWhile (== breakpoint)

nns :: Char -> Char -> Bool
nns breakpoint = (/= breakpoint)

wws :: Char -> Char -> Bool
wws breakpoint = (== breakpoint)

myPieces :: Char -> String -> [String]
myPieces breakpoint s
  | t == "" = []
  | True = (takeWhile (nns breakpoint) t) : (myPieces breakpoint (linter breakpoint (dropWhile (nns breakpoint) t)))
  where {
    t = linter breakpoint s
}

myLines :: String -> [String]
myLines = myPieces '\n'

-- Reduction should proceed as follows
--f "a b c"
--"a" : f "b c"
--"a" : "b" : f "c"
--"a" : "b" : "c" : f ""
--"a" : "b" : "c" : []
--["a", "b", "c"]
--

mySqr = [x^2 | x<- [1..5]]
myCube = [x^3 | x<- [1..5]]

squaresAndCubes = [(x, y) | x<- mySqr, y<- myCube]
smallSquaresAndCubes = [(x, y) | x<- mySqr, y<- myCube, x<50, y<50]
countSmallSquaresAndCubes = length [(x, y) | x<- mySqr, y<- myCube, x<50, y<50]




