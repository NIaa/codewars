module Foldmap where

import Data.Foldable (foldMap, Foldable)
import Data.Monoid

myToList :: Foldable t => t a -> [a]
myToList = myFoldr (:) []

myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum t = case myToList t of
    [] -> Nothing
    as -> Just $ minimum as

-- 
myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f z t = appEndo (foldMap (Endo . f) t ) z