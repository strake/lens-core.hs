{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Trustworthy #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-trustworthy-safe #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-warnings-deprecations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Zoom
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Control.Lens.Internal.Zoom
  (
  -- * Zoom
    Focusing(..)
  , FocusingWith(..)
  , FocusingPlus(..)
  , FocusingOn(..)
  , FocusingMay(..), May(..)
  , FocusingErr(..), Err(..)
--  , FocusingFree(..), Freed(..)
  -- * Magnify
  , Effect(..)
  , EffectRWS(..)
  ) where

import Control.Applicative
import Control.Category
import Control.Comonad
import Control.Monad.Reader as Reader
--import Control.Monad.Trans.Free
import qualified Data.Functor.Contravariant as Contravar
import Data.Semigroup
import Prelude hiding ((.),id)

------------------------------------------------------------------------------
-- Focusing
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into 'Control.Monad.State.StateT'.
newtype Focusing m s a = Focusing { unfocusing :: m (s, a) }
  deriving (Functor)

{-
instance (Monad m, Semigroup s) => Apply (Focusing m s) where
  Focusing mf <.> Focusing ma = Focusing $ do
    (s, f) <- mf
    (s', a) <- ma
    return (s <> s', f a)
  {-# INLINE (<.>) #-}
-}

instance (Applicative m, Monoid s) => Applicative (Focusing m s) where
  pure = Focusing . pure . pure
  {-# INLINE pure #-}
  Focusing mf <*> Focusing ma = Focusing (liftA2 (<*>) mf ma)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- FocusingWith
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into 'Control.Monad.RWS.RWST'.
newtype FocusingWith w m s a = FocusingWith { unfocusingWith :: m (s, a, w) }
  deriving (Functor)

{-
instance (Monad m, Semigroup s, Semigroup w) => Apply (FocusingWith w m s) where
  FocusingWith mf <.> FocusingWith ma = FocusingWith $ do
    (s, f, w) <- mf
    (s', a, w') <- ma
    return (s <> s', f a, w <> w')
  {-# INLINE (<.>) #-}
-}

instance (Applicative m, Monoid s, Monoid w) => Applicative (FocusingWith w m s) where
  pure a = FocusingWith (pure (mempty, a, mempty))
  {-# INLINE pure #-}
  FocusingWith mf <*> FocusingWith ma = FocusingWith $
    (\ (s, f, w) (s', a, w') -> (s <> s', f a, w <> w')) <$> mf <*> ma
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- FocusingPlus
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into 'Control.Monad.Writer.WriterT'.
newtype FocusingPlus w k s a = FocusingPlus { unfocusingPlus :: k (s, w) a }

deriving instance Functor (k (s, w)) => Functor (FocusingPlus w k s)

{-
instance Apply (k (s, w)) => Apply (FocusingPlus w k s) where
  FocusingPlus kf <.> FocusingPlus ka = FocusingPlus (kf <.> ka)
  {-# INLINE (<.>) #-}
-}

instance Applicative (k (s, w)) => Applicative (FocusingPlus w k s) where
  pure = FocusingPlus . pure
  {-# INLINE pure #-}
  FocusingPlus kf <*> FocusingPlus ka = FocusingPlus (kf <*> ka)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- FocusingOn
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into 'Control.Monad.Trans.Maybe.MaybeT' or 'Control.Monad.Trans.List.ListT'.
newtype FocusingOn f k s a = FocusingOn { unfocusingOn :: k (f s) a }
  deriving (Functor)

{-
instance Apply (k (f s)) => Apply (FocusingOn f k s) where
  FocusingOn kf <.> FocusingOn ka = FocusingOn (kf <.> ka)
  {-# INLINE (<.>) #-}
-}

instance Applicative (k (f s)) => Applicative (FocusingOn f k s) where
  pure = FocusingOn . pure
  {-# INLINE pure #-}
  FocusingOn kf <*> FocusingOn ka = FocusingOn (kf <*> ka)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- May
------------------------------------------------------------------------------

-- | Make a 'Monoid' out of 'Maybe' for error handling.
newtype May a = May { getMay :: Maybe a }

instance Semigroup a => Semigroup (May a) where
  May Nothing <> _ = May Nothing
  _ <> May Nothing = May Nothing
  May (Just a) <> May (Just b) = May (Just (a <> b))
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (May a) where
  mempty = May (Just mempty)
  {-# INLINE mempty #-}

------------------------------------------------------------------------------
-- FocusingMay
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into 'Control.Monad.Error.ErrorT'.
newtype FocusingMay k s a = FocusingMay { unfocusingMay :: k (May s) a }

deriving instance Functor (k (May s)) => Functor (FocusingMay k s)

{-
instance Apply (k (May s)) => Apply (FocusingMay k s) where
  FocusingMay kf <.> FocusingMay ka = FocusingMay (kf <.> ka)
  {-# INLINE (<.>) #-}
-}

instance Applicative (k (May s)) => Applicative (FocusingMay k s) where
  pure = FocusingMay . pure
  {-# INLINE pure #-}
  FocusingMay kf <*> FocusingMay ka = FocusingMay (kf <*> ka)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- Err
------------------------------------------------------------------------------

-- | Make a 'Monoid' out of 'Either' for error handling.
newtype Err e a = Err { getErr :: Either e a }

instance Semigroup a => Semigroup (Err e a) where
  Err (Left e) <> _ = Err (Left e)
  _ <> Err (Left e) = Err (Left e)
  Err (Right a) <> Err (Right b) = Err (Right (a <> b))
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Err e a) where
  mempty = Err (Right mempty)
  {-# INLINE mempty #-}

------------------------------------------------------------------------------
-- FocusingErr
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into 'Control.Monad.Error.ErrorT'.
newtype FocusingErr e k s a = FocusingErr { unfocusingErr :: k (Err e s) a }

deriving instance Functor (k (Err e s)) => Functor (FocusingErr e k s)

{-
instance Apply (k (Err e s)) => Apply (FocusingErr e k s) where
  FocusingErr kf <.> FocusingErr ka = FocusingErr (kf <.> ka)
  {-# INLINE (<.>) #-}
-}

instance Applicative (k (Err e s)) => Applicative (FocusingErr e k s) where
  pure = FocusingErr . pure
  {-# INLINE pure #-}
  FocusingErr kf <*> FocusingErr ka = FocusingErr (kf <*> ka)
  {-# INLINE (<*>) #-}

{-
------------------------------------------------------------------------------
-- Freed
------------------------------------------------------------------------------

-- | Make a 'Monoid' out of 'FreeF' for result collection.

newtype Freed f m a = Freed { getFreed :: FreeF f a (FreeT f m a) }

instance (Applicative f, Semigroup a, Monad m) => Semigroup (Freed f m a) where
  Freed (Pure a) <> Freed (Pure b) = Freed $ Pure $ a <> b
  Freed (Pure a) <> Freed (Free g) = Freed $ Free $ liftA2 (liftA2 (<>)) (pure $ return a) g
  Freed (Free f) <> Freed (Pure b) = Freed $ Free $ liftA2 (liftA2 (<>)) f (pure $ return b)
  Freed (Free f) <> Freed (Free g) = Freed $ Free $ liftA2 (liftA2 (<>)) f g

instance (Applicative f, Monoid a, Monad m) => Monoid (Freed f m a) where
  mempty = Freed $ Pure mempty

------------------------------------------------------------------------------
-- FocusingFree
------------------------------------------------------------------------------

-- | Used by 'Control.Lens.Zoom.Zoom' to 'Control.Lens.Zoom.zoom' into
-- 'Control.Monad.Trans.FreeT'
newtype FocusingFree f m k s a = FocusingFree { unfocusingFree :: k (Freed f m s) a }
  deriving (Functor)

{-
instance Apply (k (Freed f m s)) => Apply (FocusingFree f m k s) where
  FocusingFree kf <.> FocusingFree ka = FocusingFree (kf <.> ka)
  {-# INLINE (<.>) #-}
-}

instance Applicative (k (Freed f m s)) => Applicative (FocusingFree f m k s) where
  pure = FocusingFree . pure
  {-# INLINE pure #-}
  FocusingFree kf <*> FocusingFree ka = FocusingFree (kf <*> ka)
  {-# INLINE (<*>) #-}
-}

-----------------------------------------------------------------------------
--- Effect
-------------------------------------------------------------------------------

-- | Wrap a monadic effect with a phantom type argument.
newtype Effect m r a = Effect { getEffect :: m r }
  deriving (Functor)
-- type role Effect representational nominal phantom

instance Contravar.Functor (Effect m r) where
  gmap _ (Effect m) = Effect m
  {-# INLINE gmap #-}

instance (Applicative m, Semigroup r) => Semigroup (Effect m r a) where
  Effect ma <> Effect mb = Effect (liftA2 (<>) ma mb)
  {-# INLINE (<>) #-}

instance (Applicative m, Monoid r) => Monoid (Effect m r a) where
  mempty = Effect (pure mempty)
  {-# INLINE mempty #-}

{-
instance (Apply m, Semigroup r) => Apply (Effect m r) where
  Effect ma <.> Effect mb = Effect (liftF2 (<>) ma mb)
  {-# INLINE (<.>) #-}
-}

instance (Applicative m, Monoid r) => Applicative (Effect m r) where
  pure _ = Effect (pure mempty)
  {-# INLINE pure #-}
  Effect ma <*> Effect mb = Effect (liftA2 (<>) ma mb)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- EffectRWS
------------------------------------------------------------------------------

-- | Wrap a monadic effect with a phantom type argument. Used when magnifying 'Control.Monad.RWS.RWST'.
newtype EffectRWS w st m s a = EffectRWS { getEffectRWS :: st -> m (s,st,w) }
  deriving (Functor)

{-
instance (Semigroup s, Semigroup w, Bind m) => Apply (EffectRWS w st m s) where
  EffectRWS m <.> EffectRWS n = EffectRWS $ \st -> m st >>- \ (s,t,w) -> fmap (\(s',u,w') -> (s <> s', u, w <> w')) (n t)
  {-# INLINE (<.>) #-}
-}

instance (Monoid s, Monoid w, Monad m) => Applicative (EffectRWS w st m s) where
  pure _ = EffectRWS $ \st -> return (mempty, st, mempty)
  {-# INLINE pure #-}
  EffectRWS m <*> EffectRWS n = EffectRWS $ \st -> [(s <> s', u, w <> w') | (s,t,w) <- m st, (s',u,w') <- n t]
  {-# INLINE (<*>) #-}

instance Contravar.Functor (EffectRWS w st m s) where
  gmap _ (EffectRWS m) = EffectRWS m
  {-# INLINE gmap #-}
