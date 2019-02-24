{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE InstanceSigs #-}

module Data.Text.DrawADT.Tagged.SizedListLike (
  SizedListLike(..)
) where

--import GHC.TypeLits
import Prelude ()
import Data.Type.Nat
import Data.Kind (Type)
import qualified Data.Vec.Lazy as VL

infixr 8 `append`
class SizedListLike (sl :: Nat -> Type -> Type) where
  empty :: sl Z a
  cons :: a -> sl n a -> sl (S n) a
  singleton :: a -> sl (S Z) a
  singleton e = cons e empty
  head :: sl (S n) a -> a
  tail :: sl (S n) a -> sl n a
  map :: (a -> b) -> sl n a -> sl n b
  append :: sl n a -> sl m a -> sl (Plus n m) a
  foldr :: (a -> b -> b) -> b -> sl n a -> b

instance SizedListLike VL.Vec where
  empty = VL.empty
  cons = (VL.:::)
  singleton = VL.singleton
  head = VL.head
  tail = VL.tail
  map = VL.map
  append = (VL.++)
  foldr = VL.foldr
  