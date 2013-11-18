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

module Data.UniformPair (P(..), fstP,sndP, firstP, secondP, compareSwap) where

import Data.Monoid (Monoid(..),(<>))
import Data.Functor ((<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Control.Applicative (Applicative(..)) -- ,liftA2

infix 1 :#

-- | Uniform pairs
data P a = a :# a deriving (Show, Functor, Foldable,Traversable)

-- instance Traversable P where sequenceA (u :# v) = (:#) <$> u <*> v

fstP :: P a -> a
fstP (a :# _) = a

sndP :: P a -> a
sndP (_ :# b) = b

firstP, secondP :: (a -> a) -> (P a -> P a)
firstP  f (a :# b) = f a :# b
secondP g (a :# b) = a :# g b

-- unzipP :: Functor f => f (P a) -> P (f a)
-- unzipP ps = (fstP <$> ps) :# (sndP <$> ps)
-- unzipP = liftA2 (:#) (fmap fstP) (fmap sndP)

instance Monoid a => Monoid (P a) where
  mempty = mempty :# mempty
  (a :# b) `mappend` (c :# d) = (a <> c) :# (b <> d)  -- exchange

instance Applicative P where
  pure a = a :# a
  (f :# g) <*> (a :# b) = f a :# g b

instance Monad P where
  return = pure
  m >>= f = joinP (f <$> m)

joinP :: P (P a) -> P a
joinP ((a :# _) :# (_ :# d)) = a :# d

-- so
--
--   (a :# b) >>= f = (c :# d)
--    where
--      (c :# _) = f a
--      (_ :# d) = f b

-- Compare and swap
compareSwap :: Ord a => P a -> P a
compareSwap (a :# b) | a <= b    = a :# b
                     | otherwise = b :# a
