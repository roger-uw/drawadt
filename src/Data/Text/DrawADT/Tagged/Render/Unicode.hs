{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Text.DrawADT.Tagged.Render.Unicode (
  genUnicodeConfig,
  defaultUnicodeConfig,
  RenderUnicodeDrawT(..),
  runRenderUnicodeDrawT,
  UnicodeConfig,
  RenderUnicodeDrawTConstraints,
  UnicodeDrawT(..)
) where

import Data.Text.DrawADT.Tagged.DrawT
import Control.Monad
import Data.Type.Nat
--import GHC.TypeLits
import Control.Monad.Reader
import Data.String
import qualified Data.ListLike as LL
import qualified Data.Text.DrawADT.Tagged.SizedListLike as SL
--import Data.Kind (Type)

genUnicodeConfig :: (IsString s, LL.ListLike s Char) => String -> UnicodeConfig s
genUnicodeConfig (wireV : connL : connLL : connR : connRR : convL : convR : convLR : convU : space : wireH) =
  let len = length wireH
      spaces = replicate len space
  in UnicodeConfig {
      wireHSym = fromString wireH,
      wireVSym = fromString (spaces ++ [wireV]),
      connLSym = fromString (spaces ++ [connL]),
      connLLSym = fromString (spaces ++ [connLL]),
      connRSym = fromString (spaces ++ [connR]),
      connRRSym = fromString (spaces ++ [connRR]),
      spaceSym = fromString (space : spaces),
      convLSym = fromString [convL],
      convRSym = fromString [convR],
      convLRSym = fromString [convLR],
      convUSym = fromString [convU]
  }
genUnicodeConfig _ = error "Not a valid Unicode configuration"

defaultUnicodeConfig :: (IsString s, LL.ListLike s Char) => UnicodeConfig s
defaultUnicodeConfig = genUnicodeConfig "│┌├└├╩╦╬═ ────"

data UnicodeDrawT v l s (fl :: Nat) (nl :: Nat) (nr :: Nat) (fr :: Nat) a = UnicodeDrawT {
  farLeftT :: v fl (l s), 
  nearLeftT :: v nl (l s),
  nearRightT :: v nr (l s),
  farRightT :: v fr (l s)
}

data UnicodeConfig s = UnicodeConfig {
  wireHSym :: s,
  wireVSym :: s,
  connLSym :: s,
  connLLSym :: s,
  connRSym :: s,
  connRRSym :: s,
  spaceSym :: s,
  convLSym :: s,
  convRSym :: s,
  convLRSym :: s,
  convUSym :: s
}

data Cxt = L | LR | R | U deriving (Eq)

mergeCxt :: Cxt -> Cxt -> Cxt
mergeCxt _ U = U
mergeCxt U c = c
mergeCxt LR _ = LR
mergeCxt _ LR = LR
mergeCxt L L = L
mergeCxt L R = LR
mergeCxt R R = R
mergeCxt R L = LR

newtype RenderUnicodeDrawT v l s fl nl nr fr a = 
  RenderUnicodeDrawT (Cxt -> Reader (UnicodeConfig s) (UnicodeDrawT v l s fl nl nr fr a))

type RenderUnicodeDrawTConstraints v l s = 
  (IsString s, SL.SizedListLike v, LL.ListLike (l s) s, LL.ListLike s Char)

instance (RenderUnicodeDrawTConstraints v l s) => DrawT (RenderUnicodeDrawT v l s) where
  lineT _ s = RenderUnicodeDrawT . const $ do
    wireH <- wireHSym <$> ask
    return $ UnicodeDrawT SL.empty SL.empty (SL.singleton . LL.cons wireH . LL.singleton . fromString $ s) SL.empty
  RenderUnicodeDrawT d1 +> RenderUnicodeDrawT d2 = RenderUnicodeDrawT $ \cxt -> do
    UnicodeDrawT fl1 nl1 nr1 fr1 <- d1 (mergeCxt cxt R)
    UnicodeDrawT fl2 nl2 nr2 fr2 <- d2 (mergeCxt cxt U)
    c <- ask
    let wireV = wireVSym c
        space = spaceSym c
        conn = if cxt == R || cxt == LR then connRRSym c else connRSym c
        wireR = if cxt == R || cxt == LR then wireV else space
    return $ UnicodeDrawT {
      farLeftT = fl1, 
      nearLeftT = nl1,
      nearRightT = nr1
          `SL.append` fr1 
          `SL.append` SL.map (LL.cons wireV) fl2
          `SL.append` SL.map (LL.cons wireV) nl2
          `SL.append` SL.singleton (LL.cons conn (SL.head nr2)),
      farRightT = SL.map (LL.cons wireR) (SL.tail nr2)
          `SL.append` SL.map (LL.cons wireR) fr2
    }
  RenderUnicodeDrawT d1 <+ RenderUnicodeDrawT d2 = RenderUnicodeDrawT $ \cxt -> do
    UnicodeDrawT fl1 nl1 nr1 fr1 <- d1 (mergeCxt cxt U)
    UnicodeDrawT fl2 nl2 nr2 fr2 <- d2 (mergeCxt cxt L)
    c <- ask
    let wireV = wireVSym c
        space = spaceSym c
        conn = if cxt == L || cxt == LR then connLLSym c else connLSym c
        wireL = if cxt == L || cxt == LR then wireV else space
    return $ UnicodeDrawT {
      farLeftT =      SL.map (LL.cons wireL) fl1
          `SL.append` SL.map (LL.cons wireL) nl1,
      nearLeftT =     LL.cons conn (SL.head nr1)
          `SL.cons`   SL.map (LL.cons wireV) (SL.tail nr1)
          `SL.append` SL.map (LL.cons wireV) fr1
          `SL.append` fl2
          `SL.append` nl2,
      nearRightT = nr2,
      farRightT = fr2
    }

instance (RenderUnicodeDrawTConstraints v l s) => BlockDrawT (RenderUnicodeDrawT v l s) where
  blockT _ (RenderUnicodeDrawT d) = RenderUnicodeDrawT $ \cxt -> do
    UnicodeDrawT fl nl nr fr <- d (mergeCxt cxt U)
    c <- ask
    let wireV = wireVSym c
        wireH = wireHSym c
        conv
          | cxt == L = convLSym c
          | cxt == R = convRSym c
          | cxt == LR = convLRSym c
          | otherwise = convUSym c
        space = spaceSym c
        wireL = if cxt == L || cxt == LR then wireV else space
        wireR = if cxt == R || cxt == LR then wireV else space
    return $ UnicodeDrawT {
      farLeftT =         SL.map (LL.cons wireL) fl
             `SL.append` SL.map (LL.cons wireL) nl,
      nearLeftT = SL.empty,
      nearRightT = SL.singleton (LL.cons wireH (LL.cons conv (SL.head nr))),
      farRightT =        SL.map (LL.cons wireR) (SL.tail nr)
             `SL.append` SL.map (LL.cons wireR) fr
    }

runRenderUnicodeDrawT :: (RenderUnicodeDrawTConstraints v l s) =>
  UnicodeConfig s -> RenderUnicodeDrawT v l s fl nl nr fr a -> s
runRenderUnicodeDrawT c (RenderUnicodeDrawT d) = flattenUnicodeDrawT (runReader (d U) c)

flattenUnicodeDrawT :: forall v l s fl nl nr fr a. (RenderUnicodeDrawTConstraints v l s) =>
  UnicodeDrawT v l s fl nl nr fr a -> s
flattenUnicodeDrawT (UnicodeDrawT fl nl nr fr) = 
  let all :: v (Plus fl (Plus nl (Plus nr fr))) s
      all =         SL.map LL.concat fl 
        `SL.append` SL.map LL.concat nl
        `SL.append` SL.map LL.concat nr
        `SL.append` SL.map LL.concat fr
  in SL.foldr (\now acc -> LL.append now (LL.append "\n" acc)) "" all
