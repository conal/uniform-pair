{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.UniformPair
-- Copyright   :  (c) 2013 Tabula, Inc.
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Uniform pairs. Because these pairs memoize functions (from Bool)--i.e.,
-- they're representable functors--these instances provided are fully determined
-- by the corresponding instances for functions, thanks to the type class
-- morphism principle.
----------------------------------------------------------------------

module Data.UniformPair
  ( Pair(..), fstP,sndP, firstP, secondP, updateP, compareSwap
  ) where

import Data.Monoid (Monoid(..),(<>))
import Data.Functor ((<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Control.Applicative (Applicative(..)) -- ,liftA2

import Text.ShowF (ShowF(..))

infix 1 :#

-- | Uniform pairs
data Pair a = a :# a deriving (Eq, Ord, Show, Functor, Foldable,Traversable)

-- instance Traversable Pair where sequenceA (u :# v) = (:#) <$> u <*> v

instance ShowF Pair where
  showsPrecF = showsPrec

fstP :: Pair a -> a
fstP (a :# _) = a

sndP :: Pair a -> a
sndP (_ :# b) = b

firstP, secondP :: (a -> a) -> (Pair a -> Pair a)
firstP  f (a :# b) = f a :# b
secondP g (a :# b) = a :# g b

-- unzipP :: Functor f => f (Pair a) -> Pair (f a)
-- unzipP ps = (fstP <$> ps) :# (sndP <$> ps)
-- unzipP = liftA2 (:#) (fmap fstP) (fmap sndP)

instance Monoid a => Monoid (Pair a) where
  mempty = mempty :# mempty
  (a :# b) `mappend` (c :# d) = (a <> c) :# (b <> d)  -- exchange

instance Applicative Pair where
  pure a = a :# a
  (f :# g) <*> (a :# b) = f a :# g b

instance Monad Pair where
  return = pure
  m >>= f = joinP (f <$> m)

joinP :: Pair (Pair a) -> Pair a
joinP ((a :# _) :# (_ :# d)) = a :# d

-- so
--
--   (a :# b) >>= f = (c :# d)
--    where
--      (c :# _) = f a
--      (_ :# d) = f b

-- Update a component, indexing by 'False' for the first element and 'True' for
-- the second.
updateP :: Bool -> (a -> a) -> Pair a -> Pair a
updateP False f (a :# b) = f a :# b
updateP True  f (a :# b) = a :# f b

-- -- Untrie. What argument order do I want here?
-- getP :: Pair a -> Bool -> a
-- getP (a :# _) False = a
-- getP (_ :# b) True  = b

-- Compare and swap
compareSwap :: Ord a => Pair a -> Pair a
compareSwap (a :# b) | a <= b    = a :# b
                     | otherwise = b :# a
