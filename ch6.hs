data TisAnInteger = TisAn Integer

instance Eq (TisAnInteger) where
  TisAn x == TisAn y = (x==y)

data TwoIntegers = Two Integer Integer

instance Eq (TwoIntegers) where
  (Two a b) == (Two c d) = (a==c) && (b==d)


data StringOrInt = TisAnInt Int
      | TisAString String

instance Eq (StringOrInt) where
  TisAnInt x == TisAnInt y = (x==y)
  TisAString x == TisAString y = (x==y)
  _ == _ = False

data Pair a =
  Pair a a

instance (Eq a) => Eq (Pair a) where
  Pair x y == Pair a b = (x==a) && (y==b)

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple a b) == (Tuple x y) = (a==x) && (b==y)

data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
  ThisOne x == ThisOne y = x==y
  ThatOne x == ThatOne y = x==y
  _ == _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello x == Hello y = x==y
  Goodbye x == Goodbye y = x==y
  _ == _ = False

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun 
  deriving (Ord, Show, Eq)

data Person = Person Bool 

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

instance Show Person where
  show (Person x) = "Person " ++ (show x)

data Mood = Blah | Woot 
  deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String 
  deriving (Eq, Show)

data Yeah = Yeah Bool 
  deriving (Eq, Show)

data Papu = Papu Rocks Yeah 
  deriving (Eq, Show)

instance Ord Papu where
  Papu (Rocks x) (Yeah b) < Papu (Rocks y) (Yeah c) = (x<y) || (b<c)
  Papu (Rocks x) (Yeah b) > Papu (Rocks y) (Yeah c) = (x>y) || (b>c)
  Papu (Rocks x) (Yeah b) <= Papu (Rocks y) (Yeah c) = (x<=y) || (b<=c)
  Papu (Rocks x) (Yeah b) >= Papu (Rocks y) (Yeah c) = (x>=y) || (b>=c)
  (Papu (Rocks x) (Yeah b)) `compare` (Papu (Rocks y) (Yeah c)) = (x `compare` y) -- || (b `compare` c)


phew = Papu (Rocks "chases") (Yeah True)

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'


chk :: Eq b => (a -> b) -> a -> b -> Bool 
chk = \f -> \x -> \y -> (f x) == y

-- Hint: use some arithmetic operation to
-- -- combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b 
arith = 
  \f -> 
    \x -> 
      \i -> 
        (f i) + fromInteger x


