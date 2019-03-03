{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Extend where

import Data.Type.Nat
import Data.Kind (Type)
import Data.Text.DrawADT.Tagged.DrawT
import Data.Text.DrawADT.Draw
import Data.Text.DrawADT.Render.Wrap
import Data.Text.DrawADT.Tagged.Render.Unicode

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