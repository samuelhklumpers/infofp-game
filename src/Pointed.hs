{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}

module Pointed where

import Control.Lens


-- list with a selected point (the player in the list of all beings)
data Pointed a = Pointed {_player :: a, _npo :: [a]}
makeLenses ''Pointed

toList :: Pointed a -> [a]
toList (Pointed x xs) = x:xs

fromList :: [a] -> Pointed a
fromList (x:xs) = Pointed x xs
fromList _ = error "empty list cannot be Pointed"

instance Functor Pointed where
    fmap f = fromList . fmap f . toList

instance Semigroup (Pointed a) where
    p <> q = fromList $ toList p <> toList q

deriving instance Show a => Show (Pointed a)

terminal :: a -> Pointed a
terminal x = Pointed x []