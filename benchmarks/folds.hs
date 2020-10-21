{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

import qualified Data.Foldable as F
import           Data.Functor.Contravariant (phantom)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.Vector.Generic.Lens

import Control.Lens
import Criterion.Main
import Criterion.Types

main :: IO ()
main = defaultMainWith config
  [
    bgroup "vector"
    [ bgroup "toList"
      [ bench "native" $ nf V.toList v
      , bench "each"   $ nf (toListOf traverse) v
      ]
    ]
  , bgroup "sequence"
    [ bgroup "toList"
      [ bench "native" $ nf F.toList s
      , bench "each"   $ nf (toListOf traverse) s
      ]
    ]
  , bgroup "list"
    [ bgroup "toList"
      [ bench "native" $ nf F.toList l
      , bench "each"   $ nf (toListOf traverse) l
      ]
    ]
  , bgroup "map"
    [ bgroup "toList"
      [ bench "native" $ nf M.toList m
      , bench "each"   $ nf (toListOf (fmap phantom . M.traverseWithKey . curry)) m
      ]
    ]
  , bgroup "hash map"
    [ bgroup "toList"
      [ bench "native" $ nf HM.toList h
      , bench "each"   $ nf (toListOf (fmap phantom . HM.traverseWithKey . curry)) h
      ]
    , bgroup "sum"
      [ bench "native" $ nf (sum . id . F.toList) h
      , bench "each"   $ nf (sumOf traverse) h
      ]
    ]
  ]
  where
    config = defaultConfig { timeLimit = 1 }
    l = [0..10000] :: [Int]
    h = HM.fromList $ zip l l
    m = M.fromList $ zip l l
    s = S.fromList l
    u = U.fromList l
    v = V.fromList l
