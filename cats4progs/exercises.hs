{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

class Category obj mor | mor -> obj where
    dom :: mor -> obj
    cod :: mor -> obj
    idy :: obj -> mor
    cmp :: mor -> mor -> Maybe mor

instance Monoid mor => (Category () mor) where
    dom _ = ()
    cod _ = ()
    idy _ = mempty
    cmp m n = Just (m <> n)

instance Category onetwo fg where
    dom f = one
    dom g = two
    cod f = two
    cod g = one
    idy one = one
    idy two = two
    cmp a b = Just (a . b)
-- ?????