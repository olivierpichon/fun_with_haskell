data AnyList t = Empty | Cons t (AnyList t)
  deriving Show

mapList :: (t -> t) -> AnyList t -> AnyList t
mapList _ Empty = Empty
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

exampleList = Cons (7) (Cons 12 (Cons (16) Empty))

zipList :: AnyList t1 -> AnyList t2 -> AnyList (t1,t2)
zipList Empty Empty = Empty
zipList Empty (Cons _ _) = Empty
zipList (Cons _ _) Empty = Empty
zipList (Cons x xs) (Cons x2 xs2) = Cons (x,x2) (zipList xs xs2)

indexedList :: Integer -> Integer -> AnyList Integer
indexedList a b
  | a == b = Cons b Empty
  | otherwise   = Cons a (indexedList (a + 1) b)

sizeList :: AnyList t -> Integer
sizeList Empty = 0
sizeList (Cons _ xs) = 1 + (sizeList xs)

filterList :: (t -> Bool) -> AnyList t -> AnyList t
filterList _ Empty = Empty
filterList f (Cons x xs)
  | f x = Cons x (filterList f xs)
  | otherwise = filterList f xs

data Maybee t = NilVal | Val t
  deriving Show

detectList :: (t -> Bool) -> AnyList t -> Maybee t
detectList _ Empty = NilVal
detectList f (Cons x xs)
  | f x = Val x
  | otherwise = detectList f xs

maybeFst :: Maybee (Integer, b) -> Integer
maybeFst NilVal = -1
maybeFst (Val x) = fst x

at :: AnyList Integer -> Integer -> Integer
at x i = maybeFst (detectList ((==i) . snd) (zipList x (indexedList 1 (sizeList x))))


