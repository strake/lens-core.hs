{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 711
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Fold
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Fold
  (
  -- * Monoids for folding
    Folding(..)
  , Traversed(..)
  , TraversedF(..)
  , Sequenced(..)
  , Max(..), getMax
  , Min(..), getMin
  , Leftmost(..), getLeftmost
  , Rightmost(..), getRightmost
--  , ReifiedMonoid(..)
  -- * Semigroups for folding
  , NonEmptyDList(..)
  ) where

import Control.Applicative
import Control.Lens.Internal.Getter
--import Data.Functor.Bind
import qualified Data.Functor.Contravariant as Contravar
import Data.Maybe
import Data.Semigroup hiding (Min, getMin, Max, getMax)
--import Data.Reflection
import Prelude

import qualified Data.List.NonEmpty as NonEmpty

#ifdef HLINT
{-# ANN module "HLint: ignore Avoid lambda" #-}
#endif

------------------------------------------------------------------------------
-- Folding
------------------------------------------------------------------------------

-- | A 'Monoid' for a 'Contravar.Functor' 'Applicative'.
newtype Folding f a = Folding { getFolding :: f a }

instance (Contravar.Functor f, Applicative f) => Semigroup (Folding f a) where
  Folding fr <> Folding fs = Folding (fr *> fs)
  {-# INLINE (<>) #-}

instance (Contravar.Functor f, Applicative f) => Monoid (Folding f a) where
  mempty = Folding noEffect
  {-# INLINE mempty #-}

------------------------------------------------------------------------------
-- Traversed
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.traverseOf_' and the like.
--
-- The argument 'a' of the result should not be used!
newtype Traversed a f = Traversed { getTraversed :: f a }

-- See 4.16 Changelog entry for the explanation of "why not Apply f =>"?
instance Applicative f => Semigroup (Traversed a f) where
  Traversed ma <> Traversed mb = Traversed (ma *> mb)
  {-# INLINE (<>) #-}

------------------------------------------------------------------------------
-- TraversedF
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Fold.traverse1Of_' and the like.
--
-- @since 4.16
newtype TraversedF a f = TraversedF { getTraversedF :: f a }

instance Applicative f => Semigroup (TraversedF a f) where
  TraversedF ma <> TraversedF mb = TraversedF (ma *> mb)
  {-# INLINE (<>) #-}

------------------------------------------------------------------------------
-- Sequenced
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.mapM_' and the like.
--
-- The argument 'a' of the result should not be used!
--
-- See 4.16 Changelog entry for the explanation of "why not Apply f =>"?
newtype Sequenced a m = Sequenced { getSequenced :: m a }

instance Monad m => Semigroup (Sequenced a m) where
  Sequenced ma <> Sequenced mb = Sequenced (ma >> mb)
  {-# INLINE (<>) #-}

------------------------------------------------------------------------------
-- Min
------------------------------------------------------------------------------

-- | Used for 'Control.Lens.Fold.minimumOf'.
data Min a = NoMin | Min a

instance Ord a => Semigroup (Min a) where
  NoMin <> m     = m
  m <> NoMin     = m
  Min a <> Min b = Min (min a b)
  {-# INLINE (<>) #-}

instance Ord a => Monoid (Min a) where
  mempty = NoMin
  {-# INLINE mempty #-}

-- | Obtain the minimum.
getMin :: Min a -> Maybe a
getMin NoMin   = Nothing
getMin (Min a) = Just a
{-# INLINE getMin #-}

------------------------------------------------------------------------------
-- Max
------------------------------------------------------------------------------

-- | Used for 'Control.Lens.Fold.maximumOf'.
data Max a = NoMax | Max a

instance Ord a => Semigroup (Max a) where
  NoMax <> m = m
  m <> NoMax = m
  Max a <> Max b = Max (max a b)
  {-# INLINE (<>) #-}

instance Ord a => Monoid (Max a) where
  mempty = NoMax
  {-# INLINE mempty #-}

-- | Obtain the maximum.
getMax :: Max a -> Maybe a
getMax NoMax   = Nothing
getMax (Max a) = Just a
{-# INLINE getMax #-}

------------------------------------------------------------------------------
-- NonEmptyDList
------------------------------------------------------------------------------

newtype NonEmptyDList a
  = NonEmptyDList { getNonEmptyDList :: [a] -> NonEmpty.NonEmpty a }

instance Semigroup (NonEmptyDList a) where
  NonEmptyDList f <> NonEmptyDList g = NonEmptyDList (f . NonEmpty.toList . g)

------------------------------------------------------------------------------
-- Leftmost and Rightmost
------------------------------------------------------------------------------

-- | Used for 'Control.Lens.Fold.firstOf'.
data Leftmost a = LPure | LLeaf a | LStep (Leftmost a)

instance Semigroup (Leftmost a) where
  x <> y = LStep $ case x of
    LPure    -> y
    LLeaf _  -> x
    LStep x' -> case y of
      -- The last two cases make firstOf produce a Just as soon as any element
      -- is encountered, and possibly serve as a micro-optimisation; this
      -- behaviour can be disabled by replacing them with _ -> x <> y'.
      -- Note that this means that firstOf (backwards folded) [1..] is Just _|_.
      LPure    -> x'
      LLeaf a  -> LLeaf $ fromMaybe a (getLeftmost x')
      LStep y' -> x' <> y'
  {-# INLINE (<>) #-}

instance Monoid (Leftmost a) where
  mempty = LPure
  {-# INLINE mempty #-}

-- | Extract the 'Leftmost' element. This will fairly eagerly determine that it can return 'Just'
-- the moment it sees any element at all.
getLeftmost :: Leftmost a -> Maybe a
getLeftmost LPure = Nothing
getLeftmost (LLeaf a) = Just a
getLeftmost (LStep x) = getLeftmost x

-- | Used for 'Control.Lens.Fold.lastOf'.
data Rightmost a = RPure | RLeaf a | RStep (Rightmost a)

instance Semigroup (Rightmost a) where
  x <> y = RStep $ case y of
    RPure    -> x
    RLeaf _  -> y
    RStep y' -> case x of
      -- The last two cases make lastOf produce a Just as soon as any element
      -- is encountered, and possibly serve as a micro-optimisation; this
      -- behaviour can be disabled by replacing them with _ -> x <> y'.
      -- Note that this means that lastOf folded [1..] is Just _|_.
      RPure    -> y'
      RLeaf a  -> RLeaf $ fromMaybe a (getRightmost y')
      RStep x' -> x' <> y'
  {-# INLINE (<>) #-}

instance Monoid (Rightmost a) where
  mempty = RPure
  {-# INLINE mempty #-}

-- | Extract the 'Rightmost' element. This will fairly eagerly determine that it can return 'Just'
-- the moment it sees any element at all.
getRightmost :: Rightmost a -> Maybe a
getRightmost RPure = Nothing
getRightmost (RLeaf a) = Just a
getRightmost (RStep x) = getRightmost x
