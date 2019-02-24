{-# LANGUAGE FlexibleContexts #-}

module Data.Text.DrawADT.Render.Unicode (
  module Data.Text.DrawADT.Render.Unicode
) where

import Data.Text.DrawADT.Render.Wrap
import Data.Text.DrawADT.Tagged.Render

type RenderUnicodeDraw v l s a = Wrap (RenderUnicodeDrawT v l s) a

runRenderUnicodeDraw :: (RenderUnicodeDrawTConstraints v l s) => UnicodeConfig s -> RenderUnicodeDraw v l s a -> s
runRenderUnicodeDraw c = runWrap (runRenderUnicodeDrawT c)