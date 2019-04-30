{-# LANGUAGE TemplateHaskell #-}
module Kata.TupleMaker (tuple) where

import Control.Monad
import Language.Haskell.TH

tuple :: Int -> Q Exp
tuple n = do
  ns <- replicateM n (newName "x")
  lamE (map varP ns) (tupE $ map varE ns)

-- edge case already included.
-- names don't really matter.
{-
tuple :: Int -> Q Exp
tuple 0 = lift ()
tuple 1 = [| id |]
tuple n = return $ LamE (VarP <$> as) (TupE $ VarE <$> as) where 
  as = [ mkName $ "a" ++ show i | i <- [1..n] ]
-}