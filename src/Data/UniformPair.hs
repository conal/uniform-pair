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

-- TODO: Consider more lazy patterns so that we don't force the argument pair
-- structure before revealing the result pair structure. Candidates: toP, fromP,
-- update, mappend, (<*>), joinP, reverse, onElemP, and compareSwap. The mappend
-- on (a,b) look like it has this same eager in base 4.8.

module Data.UniformPair
  ( Pair(..), fstP,sndP, firstP, secondP, getP, onElemP, swapP, compareSwap
  ) where

import Data.Monoid (Monoid(..))
import Data.Functor ((<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Control.Applicative (Applicative(..)) -- ,liftA2
import Control.DeepSeq (NFData(..))

import Prelude.Extras (Show1(..))

infix 1 :#

-- | Uniform pairs
data Pair a = a :# a deriving (Eq, Ord, Show, Functor, Foldable,Traversable)

-- instance Traversable Pair where sequenceA (u :# v) = (:#) <$> u <*> v

instance NFData a => NFData (Pair a) where
    rnf (a :# b) = rnf a `seq` rnf b

instance Show1 Pair

fstP :: Pair a -> a
fstP (a :# _) = a

sndP :: Pair a -> a
sndP (_ :# b) = b

firstP, secondP :: (a -> a) -> (Pair a -> Pair a)
firstP  f ~(a :# b) = f a :# b
secondP g ~(a :# b) = a :# g b

-- unzipP :: Functor f => f (Pair a) -> Pair (f a)
-- unzipP ps = (fstP <$> ps) :# (sndP <$> ps)
-- unzipP = liftA2 (:#) (fmap fstP) (fmap sndP)

instance Monoid a => Monoid (Pair a) where
  mempty = mempty :# mempty
  (a :# b) `mappend` (c :# d) = (a `mappend` c) :# (b `mappend` d)  -- exchange

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

-- | Update a component, indexing by 'False' for the first element and 'True' for
-- the second.
onElemP :: Bool -> (a -> a) -> Pair a -> Pair a
onElemP c f ~(a :# b) | c         = f a :# b
                      | otherwise = a :# f b

-- -- Too strict:
-- onElemP False f (a :# b) = f a :# b
-- onElemP True  f (a :# b) = a :# f b

-- onElemP False = \ f (a :# b) -> f a :# b
-- onElemP True  = \ f (a :# b) -> a :# f b

-- | Extract an element, indexing by 'False' for the first element and 'True'
-- for the second.
getP :: Bool -> Pair a -> a
getP False = fstP
getP True  = sndP

-- | Swap the elements of a 'Pair'
swapP :: Pair a -> Pair a
swapP (a :# b) = b :# a

-- Compare and swap
compareSwap :: Ord a => Pair a -> Pair a
compareSwap (a :# b) | a <= b    = a :# b
                     | otherwise = b :# a
