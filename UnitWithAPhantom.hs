module UnitWithAPhantom where

data UnitWithAPhantom a = UnitWithAPhantom
    deriving (Show, Eq, Ord)

instance Functor UnitWithAPhantom where
    fmap _ _   = UnitWithAPhantom

instance Applicative UnitWithAPhantom where
    (<*>) _ _  = UnitWithAPhantom
    pure _     = UnitWithAPhantom

instance Monad UnitWithAPhantom where
    (>>=) _ _  = UnitWithAPhantom
    return = pure
    (>>)   = (*>)

