{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

module Extend where

import Data.Type.Nat
import Data.Kind (Type)
import Data.Text.DrawADT.Tagged.DrawT
import Data.Text.DrawADT.Draw
import Data.Text.DrawADT.Render.Wrap
import Data.Text.DrawADT.Tagged.Render.Unicode
import qualified Data.Text.Lazy as T
import qualified Data.ListLike as LL
import qualified Data.Text.DrawADT.Tagged.SizedListLike as SL
import Control.Monad
import Control.Monad.Reader

class ApplyDrawT (repr :: Nat -> Nat -> Nat -> Nat -> Type -> Type) where
  applyT :: (a -> b) -> repr fl nl (S nr) fr a -> repr fl nl (S nr) fr b

instance ApplyDrawT IDDrawT where
  applyT f (IDDrawT a) = IDDrawT (f a)

instance (RenderUnicodeDrawTConstraints v l s) => ApplyDrawT (RenderUnicodeDrawT v l s) where
  applyT _ (RenderUnicodeDrawT d) = RenderUnicodeDrawT $ \cxt -> do
    UnicodeDrawT fl nl nr fr <- d cxt
    return (UnicodeDrawT fl nl nr fr)

class ApplyDraw (repr :: Type -> Type) where
  apply :: (a -> b) -> repr a -> repr b

instance ApplyDraw IDDraw where
  apply f (IDDraw a) = IDDraw (f a)

instance (ApplyDrawT repr) => ApplyDraw (Wrap repr) where
  apply f (Wrap d) = Wrap (applyT f d)

class NamedBlockDrawT (repr :: Nat -> Nat -> Nat -> Nat -> Type -> Type) where
  namedBlockT :: (a -> b) -> String -> repr fl nl (S nr) fr a -> repr (Plus fl nl) Z (S Z) (Plus nr fr) b

instance (RenderUnicodeDrawTConstraints v l T.Text) => NamedBlockDrawT (RenderUnicodeDrawT v l T.Text) where
  namedBlockT _ s (RenderUnicodeDrawT d) = RenderUnicodeDrawT $ \cxt -> do
    UnicodeDrawT fl nl nr fr <- d (mergeCxt cxt U)
    c <- ask
    let wireV = wireVSym c
        wireH = wireHSym c
        space = spaceSym c
        s' = T.pack s
        extraSpace = let x = T.head space in T.map (const x) (T.tail s')
        wireL = if cxt == L || cxt == LR then wireV else space
        wireR = if cxt == R || cxt == LR then wireV else space
    return $ UnicodeDrawT {
      farLeftT =         SL.map (LL.cons wireL . LL.cons extraSpace) fl
             `SL.append` SL.map (LL.cons wireL . LL.cons extraSpace) nl,
      nearLeftT = SL.empty,
      nearRightT = SL.singleton (LL.cons wireH (LL.cons s' (SL.head nr))),
      farRightT =        SL.map (LL.cons wireR . LL.cons extraSpace) (SL.tail nr)
             `SL.append` SL.map (LL.cons wireR . LL.cons extraSpace) fr
    }

class NamedBlockDraw (repr :: Type -> Type) where
  namedBlock :: (a -> b) -> String -> repr a -> repr b

instance (NamedBlockDrawT repr) => NamedBlockDraw (Wrap repr) where
  namedBlock f s (Wrap d) = Wrap (namedBlockT f s d)