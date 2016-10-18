munge :: (x -> y) -> (y -> (w,z)) -> x -> w
munge f g = first . g . f

first :: (a,b) -> a
first (x,_) = x


