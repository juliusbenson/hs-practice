{-# LANGUAGE DeriveFunctor #-}

newtype Fix f = Fix (f (Fix f))

data StreamF e a = StreamF e a
    deriving Functor

data Stream e = Stream e (Stream e)

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

era :: [Int] -> StreamF Int [Int]
era (p : ns) = StreamF p (filter (notdiv p) ns)
    where notdiv p n = n `mod` p /= 0

primes = ana era [2..]

toListC :: Fix (StreamF e) -> [e]
toListC = cata al
    where al :: StreamF e [e] -> [e]
          al (StreamF e a) = e : a