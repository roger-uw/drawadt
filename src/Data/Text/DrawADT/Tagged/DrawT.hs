{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE ExplicitForAll #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FunctionalDependencies #-}

module Data.Text.DrawADT.Tagged.DrawT (
  DrawT(..),
  BlockDrawT(..),
  IDDrawT(..)
) where

--import GHC.TypeLits
import Data.Type.Nat
import Data.Kind (Type)
import Data.Proxy

infixl 8 +>
infixr 9 <+
class DrawT (repr :: Nat -> Nat -> Nat -> Nat -> Type -> Type) where
  lineT :: a -> String -> repr Z Z (S Z) Z a
  (+>) :: repr fl nl (S nr) fr (a -> b)
          -> repr fl' nl' (S nr') fr' a
          -> repr fl nl (Plus (S nr) (Plus fr (Plus fl' (Plus nl' (S Z))))) (Plus nr' fr') b
  (<+) :: repr fl nl (S nr) fr a
          -> repr fl' nl' (S nr') fr' (a -> b)
          -> repr (Plus fl nl) (Plus (S nr) (Plus fr (Plus fl' nl'))) (S nr') fr' b

class BlockDrawT (repr :: Nat -> Nat -> Nat -> Nat -> Type -> Type) where
  blockT :: (a -> b) -> repr fl nl (S nr) fr a -> repr (Plus fl nl) Z (S Z) (Plus nr fr) b

newtype IDDrawT (fl :: Nat) (nl :: Nat) (nr :: Nat) (fr :: Nat) a = IDDrawT {runIDDrawT :: a}

instance DrawT IDDrawT where
  lineT a _ = IDDrawT a
  (IDDrawT ab) +> (IDDrawT a) = IDDrawT (ab a)
  (IDDrawT a) <+ (IDDrawT ab) = IDDrawT (ab a)

instance BlockDrawT IDDrawT where
  blockT f (IDDrawT a) = IDDrawT (f a)