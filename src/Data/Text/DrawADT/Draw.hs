{-# LANGUAGE KindSignatures #-}

module Data.Text.DrawADT.Draw (
  Draw(..),
  BlockDraw(..),
  IDDraw(..)
) where

infixl 8 ++>
infixr 9 <++
class Draw (repr :: * -> *) where
  line :: a -> String -> repr a
  (++>) :: repr (a -> b) -> repr a -> repr b
  (<++) :: repr a -> repr (a -> b) -> repr b

class BlockDraw (repr :: * -> *) where
  block :: (a -> b) -> repr a -> repr b

newtype IDDraw a = IDDraw {runIDDraw :: a}

instance Draw IDDraw where
  line a _ = IDDraw a
  IDDraw ab ++> IDDraw a = IDDraw (ab a)
  IDDraw a <++ IDDraw ab = IDDraw (ab a)

instance BlockDraw IDDraw where
  block f (IDDraw a) = IDDraw (f a)
