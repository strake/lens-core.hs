{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.Vector.Generic.Lens

import Control.Lens
import Criterion.Main
import Criterion.Types

#if !(MIN_VERSION_containers(0,5,0))
import qualified Data.Foldable as F
#endif

#if !(MIN_VERSION_containers(0,5,0))
-- Sadly, containers doesn't export the constructor for Seq on older versions,
-- so we'll have to settle for this inefficient implementation of rnf.
instance NFData a => NFData (S.Seq a) where
    rnf = rnf . F.toList
#endif

main :: IO ()
main = defaultMainWith config
  [
    bgroup "vector"
    [ bgroup "map"
      [ bench "native"     $ nf (V.map (+100)) v
      , bench "each"       $ nf (over traverse (+100)) v
      ]
    ]
  , bgroup "sequence"
    [ bgroup "map"
      [ bench "native" $ nf (fmap            (+100)) s
      , bench "each"   $ nf (over traverse   (+100)) s
      ]
    ]
  , bgroup "list"
    [ bgroup "map"
      [ bench "native" $ nf (map           (+100)) l
      , bench "each"   $ nf (over traverse (+100)) l
      ]
    ]
  , bgroup "map"
    [ bgroup "map"
      [ bench "native"     $ nf (fmap            (+100)) m
      , bench "each"       $ nf (over traverse   (+100)) m
      ]
    ]
  , bgroup "hash map"
    [ bgroup "map"
      [ bench "native" $ nf (HM.map    (+100)) h
      , bench "each"   $ nf (over traverse (+100)) h
      ]
    ]
  ]
  where
    config = defaultConfig { timeLimit = 1 }
    l  = [0..10000] :: [Int]
    xl = [0..100000] :: [Int]
    h  = HM.fromList $ zip l l
    m  = M.fromList $ zip l l
    s  = S.fromList l
    u  = U.fromList xl
    v  = V.fromList l
