module Data.Profunctor.Unsafe where

import Data.Profunctor

infixr 9 #.
infixl 8 .#

(#.) :: Profunctor p => (b -> c) -> p a b -> p a c
(#.) = rmap

(.#) :: Profunctor p => (a -> b) -> p b c -> p a c
(.#) = lmap
