{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE RoleAnnotations #-}
#endif
{-# LANGUAGE StandaloneDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Magma
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Magma
  (
  -- * Magma
    Magma(..)
  , runMagma
  -- * Molten
  , Molten(..)
  -- * Mafic
  , Mafic(..)
  , runMafic
{-
  -- * TakingWhile
  , TakingWhile(..)
  , runTakingWhile
-}
  ) where

import Control.Applicative
import Control.Category
import Control.Comonad
import Control.Lens.Internal.Bazaar
import Control.Lens.Internal.Context
--import Control.Lens.Internal.Indexed
import Data.Foldable
--import Data.Functor.Apply
import qualified Data.Functor.Contravariant as Contravar
import Data.Monoid
--import Data.Profunctor.Rep
--import Data.Profunctor.Sieve
import Data.Profunctor.Unsafe
import Data.Traversable
import Prelude hiding ((.),id)

------------------------------------------------------------------------------
-- Magma
------------------------------------------------------------------------------

-- | This provides a way to peek at the internal structure of a
-- 'Control.Lens.Traversal.Traversal' or 'Control.Lens.Traversal.IndexedTraversal'
data Magma i t b a where
  MagmaAp   :: Magma i (x -> y) b a -> Magma i x b a -> Magma i y b a
  MagmaPure :: x -> Magma i x b a
  MagmaFmap :: (x -> y) -> Magma i x b a -> Magma i y b a
  Magma :: i -> a -> Magma i b b a

deriving instance Foldable (Magma i t b)
deriving instance Functor (Magma i t b)
deriving instance Traversable (Magma i t b)

#if __GLASGOW_HASKELL__ >= 707
-- note the 3rd argument infers as phantom, but that would be unsound
type role Magma representational nominal nominal nominal
#endif

instance (Show i, Show a) => Show (Magma i t b a) where
  showsPrec d (MagmaAp x y) = showParen (d > 4) $
    showsPrec 4 x . showString " <*> " . showsPrec 5 y
  showsPrec d (MagmaPure _) = showParen (d > 10) $
    showString "pure .."
  showsPrec d (MagmaFmap _ x) = showParen (d > 4) $
    showString ".. <$> " . showsPrec 5 x
  showsPrec d (Magma i a) = showParen (d > 10) $
    showString "Magma " . showsPrec 11 i . showChar ' ' . showsPrec 11 a

-- | Run a 'Magma' where all the individual leaves have been converted to the
-- expected type
runMagma :: Magma i t a a -> t
runMagma (MagmaAp l r)   = runMagma l (runMagma r)
runMagma (MagmaFmap f r) = f (runMagma r)
runMagma (MagmaPure x)   = x
runMagma (Magma _ a) = a

------------------------------------------------------------------------------
-- Molten
------------------------------------------------------------------------------

-- | This is a a non-reassociating initially encoded version of 'Bazaar'.
newtype Molten i a b t = Molten { runMolten :: Magma i t b a }

instance Functor (Molten i a b) where
  fmap f (Molten xs) = Molten (MagmaFmap f xs)
  {-# INLINE fmap #-}

{-
instance Apply (Molten i a b) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}
-}

instance Applicative (Molten i a b) where
  pure  = Molten #. MagmaPure
  {-# INLINE pure #-}
  Molten xs <*> Molten ys = Molten (MagmaAp xs ys)
  {-# INLINE (<*>) #-}

{-
instance Sellable (Indexed i) (Molten i) where
  sell = Indexed (\i -> Molten #. Magma i)
  {-# INLINE sell #-}

instance Bizarre (Indexed i) (Molten i) where
  bazaar f (Molten (MagmaAp x y))   = bazaar f (Molten x) <*> bazaar f (Molten y)
  bazaar f (Molten (MagmaFmap g x)) = g <$> bazaar f (Molten x)
  bazaar _ (Molten (MagmaPure x))   = pure x
  bazaar f (Molten (Magma i a)) = indexed f i a
-}

instance IndexedFunctor (Molten i) where
  ifmap f (Molten xs) = Molten (MagmaFmap f xs)
  {-# INLINE ifmap #-}

instance IndexedComonad (Molten i) where
  iextract (Molten (MagmaAp x y))   = iextract (Molten x) (iextract (Molten y))
  iextract (Molten (MagmaFmap f y)) = f (iextract (Molten y))
  iextract (Molten (MagmaPure x))   = x
  iextract (Molten (Magma _ a)) = a

  iduplicate (Molten (Magma i a)) = Molten #. Magma i <$> Molten (Magma i a)
  iduplicate (Molten (MagmaPure x))   = pure (pure x)
  iduplicate (Molten (MagmaFmap f y)) = iextend (fmap f) (Molten y)
  iduplicate (Molten (MagmaAp x y))   = iextend (<*>) (Molten x) <*> iduplicate (Molten y)

  iextend k (Molten (Magma i a)) = (k . Molten) . Magma i <$> Molten (Magma i a)
  iextend k (Molten (MagmaPure x))   = pure (k (pure x))
  iextend k (Molten (MagmaFmap f y)) = iextend (k . fmap f) (Molten y)
  iextend k (Molten (MagmaAp x y))   = iextend (\x' y' -> k $ x' <*> y') (Molten x) <*> iduplicate (Molten y)

instance a ~ b => Comonad (Molten i a b) where
  copure   = iextract
  {-# INLINE copure #-}
  (<<=)    = iextend
  {-# INLINE (<<=) #-}
  cut = iduplicate
  {-# INLINE cut #-}

------------------------------------------------------------------------------
-- Mafic
------------------------------------------------------------------------------

-- | This is used to generate an indexed magma from an unindexed source
--
-- By constructing it this way we avoid infinite reassociations in sums where possible.
data Mafic a b t = Mafic Int (Int -> Magma Int t b a)

-- | Generate a 'Magma' using from a prefix sum.
runMafic :: Mafic a b t -> Magma Int t b a
runMafic (Mafic _ k) = k 0

instance Functor (Mafic a b) where
  fmap f (Mafic w k) = Mafic w (MagmaFmap f . k)
  {-# INLINE fmap #-}

{-
instance Apply (Mafic a b) where
  Mafic wf mf <.> ~(Mafic wa ma) = Mafic (wf + wa) $ \o -> MagmaAp (mf o) (ma (o + wf))
  {-# INLINE (<.>) #-}
-}

instance Applicative (Mafic a b) where
  pure a = Mafic 0 $ \_ -> MagmaPure a
  {-# INLINE pure #-}
  Mafic wf mf <*> ~(Mafic wa ma) = Mafic (wf + wa) $ \o -> MagmaAp (mf o) (ma (o + wf))
  {-# INLINE (<*>) #-}

{-
instance Sellable (->) Mafic where
  sell a = Mafic 1 $ \ i -> Magma i a
  {-# INLINE sell #-}

instance Bizarre (Indexed Int) Mafic where
  bazaar (pafb :: Indexed Int a (f b)) (Mafic _ k) = go (k 0) where
    go :: Magma Int t b a -> f t
    go (MagmaAp x y)   = go x <*> go y
    go (MagmaFmap f x) = f <$> go x
    go (MagmaPure x)   = pure x
    go (Magma i a) = indexed pafb (i :: Int) a
  {-# INLINE bazaar #-}
-}

instance IndexedFunctor Mafic where
  ifmap f (Mafic w k) = Mafic w (MagmaFmap f . k)
  {-# INLINE ifmap #-}

{-
------------------------------------------------------------------------------
-- TakingWhile
------------------------------------------------------------------------------

-- | This is used to generate an indexed magma from an unindexed source
--
-- By constructing it this way we avoid infinite reassociations where possible.
--
-- In @'TakingWhile' p g a b t@, @g@ has a @nominal@ role to avoid exposing an illegal _|_ via 'Contravar.Functor',
-- while the remaining arguments are degraded to a @nominal@ role by the invariants of 'Magma'
data TakingWhile p (g :: * -> *) a b t = TakingWhile Bool t (Bool -> Magma () t b (Corep p a))
#if __GLASGOW_HASKELL__ >= 707
type role TakingWhile nominal nominal nominal nominal nominal
#endif

-- | Generate a 'Magma' with leaves only while the predicate holds from left to right.
runTakingWhile :: TakingWhile p f a b t -> Magma () t b (Corep p a)
runTakingWhile (TakingWhile _ _ k) = k True

instance Functor (TakingWhile p f a b) where
  fmap f (TakingWhile w t k) = let ft = f t in TakingWhile w ft $ \b -> if b then MagmaFmap f (k b) else MagmaPure ft
  {-# INLINE fmap #-}

{-
instance Apply (TakingWhile p f a b) where
  TakingWhile wf tf mf <.> ~(TakingWhile wa ta ma) = TakingWhile (wf && wa) (tf ta) $ \o ->
    if o then MagmaAp (mf True) (ma wf) else MagmaPure (tf ta)
  {-# INLINE (<.>) #-}
-}

instance Applicative (TakingWhile p f a b) where
  pure a = TakingWhile True a $ \_ -> MagmaPure a
  {-# INLINE pure #-}
  TakingWhile wf tf mf <*> ~(TakingWhile wa ta ma) = TakingWhile (wf && wa) (tf ta) $ \o ->
    if o then MagmaAp (mf True) (ma wf) else MagmaPure (tf ta)
  {-# INLINE (<*>) #-}

{-
instance Corepresentable p => Bizarre p (TakingWhile p g) where
  bazaar (pafb :: p a (f b)) ~(TakingWhile _ _ k) = go (k True) where
    go :: Magma () t b (Corep p a) -> f t
    go (MagmaAp x y)  = go x <*> go y
    go (MagmaFmap f x)  = f <$> go x
    go (MagmaPure x)    = pure x
    go (Magma _ wa) = cosieve pafb wa
  {-# INLINE bazaar #-}
-}

instance IndexedFunctor (TakingWhile p f) where
  ifmap = fmap
  {-# INLINE ifmap #-}
-}
