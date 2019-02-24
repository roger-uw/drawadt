{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Text.DrawADT (
  module Data.Text.DrawADT,
  module Data.Text.DrawADT.Draw,
  module Data.Text.DrawADT.Render,
  module Data.Text.DrawADT.Tagged
) where

import Data.Text.DrawADT.Draw
import Data.Text.DrawADT.Render
import Data.Text.DrawADT.Tagged

class DrawADT repr a where
  draw :: a -> repr a

instance {-# OVERLAPPABLE #-} (Draw repr, Show a) => DrawADT repr a where
  draw a = line a (" " ++ show a)