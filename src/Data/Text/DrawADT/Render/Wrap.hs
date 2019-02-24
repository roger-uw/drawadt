{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Text.DrawADT.Render.Wrap (
  module Data.Text.DrawADT.Render.Wrap
) where

import Data.Text.DrawADT.Draw
import Data.Text.DrawADT.Tagged.DrawT
import Data.Type.Nat

data Wrap repr a = forall (fl :: Nat) (nl :: Nat) (nr :: Nat) (fr :: Nat). Wrap (repr fl nl (S nr) fr a)

runWrap :: (forall fl nl nr fr. repr fl nl nr fr a -> s) -> Wrap repr a -> s
runWrap f (Wrap d) = f d

instance (DrawT repr) => Draw (Wrap repr) where
  line a s = Wrap (lineT a s)
  Wrap d1 ++> Wrap d2 = Wrap (d1 +> d2)
  Wrap d1 <++ Wrap d2 = Wrap (d1 <+ d2)

instance (BlockDrawT repr) => BlockDraw (Wrap repr) where
  block f (Wrap d) = Wrap (blockT f d)