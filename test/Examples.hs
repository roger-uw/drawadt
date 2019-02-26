{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Examples where

import Data.Text.DrawADT
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vec.Lazy as VL
import Extend

toText :: RenderUnicodeDraw VL.Vec [] T.Text a -> T.Text
toText = runRenderUnicodeDraw defaultUnicodeConfig

printDraw :: RenderUnicodeDraw VL.Vec [] T.Text a -> IO ()
printDraw = T.putStr . toText

instance (Draw repr, Show a) => DrawADT repr [a] where
  draw = foldr f (line [] " ~")
    where f now acc = line (now :) (show now) ++> acc

testList :: [Int]
testList = [1..5]

data BinTree a = BinEmpty | BinTree a (BinTree a) (BinTree a) deriving (Show)

makeBaseFunctor ''BinTree

instance (Draw repr, Show a) => DrawADT repr (BinTree a) where
  draw = cata alg
    where alg (BinTreeF a l r) = l <++ line (BinTree a) (show a) ++> r
          alg BinEmptyF = line BinEmpty " ~"

testBinTree :: Int -> BinTree Int
testBinTree = ana coalg
  where coalg n
          | n <= 0 = BinEmptyF
          | otherwise = BinTreeF n (n - 1) (n - 2)

newtype LeafBinTree a = LeafBinTree (BinTree a)

instance (Draw repr, ApplyDraw repr, Show a) => DrawADT repr (LeafBinTree a) where
  draw (LeafBinTree t) = apply LeafBinTree (para alg t)
    where alg (BinTreeF a (BinEmpty, _) (BinEmpty, _)) = line (BinTree a BinEmpty BinEmpty) (show a)
          alg (BinTreeF a (_, l) (BinEmpty, _)) = l <++ line (flip (BinTree a) BinEmpty) (show a)
          alg (BinTreeF a (BinEmpty, _) (_, r)) = line (BinTree a BinEmpty) (show a) ++> r
          alg (BinTreeF a (_, l) (_, r)) = l <++ line (BinTree a) (show a) ++> r

data QuadTree a = QuadEmpty | QuadTree a (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a) deriving (Show)

makeBaseFunctor ''QuadTree

instance (Draw repr, Show a) => DrawADT repr (QuadTree a) where
  draw = cata alg
    where alg (QuadTreeF a fl nl nr fr) = fl <++ nl <++ line (QuadTree a) (show a) ++> nr ++> fr
          alg QuadEmptyF = line QuadEmpty " ~"

testQuadTree :: Int -> QuadTree Int
testQuadTree = ana coalg
  where coalg n
          | n <= 0 = QuadEmptyF
          | otherwise = QuadTreeF n (n - 1) (n - 2) (n - 3) (n - 4)

newtype ComplexQuadTree a = ComplexQuadTree (QuadTree a)

instance (Draw repr, BlockDraw repr, ApplyDraw repr, DrawADT repr a) => DrawADT repr (ComplexQuadTree a) where
  draw (ComplexQuadTree t) = apply ComplexQuadTree (cata alg t)
    where alg (QuadTreeF a fl nl nr fr) = fl <++ nl <++ block QuadTree (draw a) ++> nr ++> fr
          alg QuadEmptyF = line QuadEmpty " ~"

testComplexQuadTree :: Int -> QuadTree [Int]
testComplexQuadTree = ana coalg
  where coalg n
          | n <= 0 = QuadEmptyF
          | otherwise = QuadTreeF [0..n] (n - 1) (n - 2) (n - 3) (n - 4)

data RoseTree a = RoseTree a [RoseTree a] deriving (Show)

makeBaseFunctor ''RoseTree

instance (Draw repr, BlockDraw repr, DrawADT repr a) => DrawADT repr (RoseTree a) where
  draw = cata alg
    where alg (RoseTreeF a ts) = draw a <++ line (RoseTree) "Rose" ++> cata alg' ts
          alg' Nil = line [] " ~"
          alg' (Cons x xs) = xs <++ block (:) x

testRoseTree :: RoseTree Int
testRoseTree = RoseTree 5 [RoseTree 4 [], RoseTree 3 [], RoseTree 2 [RoseTree 1 [], RoseTree 0 []]]

data Op = Add | Sub | Mul | Div deriving (Show)
type Id = String
data Expr = Lit Int | BinOp Op Expr Expr | Var Id | Fun Id Expr | App Expr Expr deriving (Show)

makeBaseFunctor ''Expr

instance (Draw repr, BlockDraw repr) => DrawADT repr Expr where
  draw = cata alg
    where alg (LitF n) = line (Lit n) ("Lit " ++ show n)
          alg (BinOpF op e0 e1) = e0 <++ line (BinOp op) (show op) ++> e1
          alg (VarF i) = line (Var i) ("Var " ++ i)
          alg (FunF i e) = line (Fun i) ("@ " ++ i) ++> e
          alg (AppF e0 e1) = e0 <++ line App "App" ++> e1

testExpr :: Expr
testExpr = App (Fun "x" (BinOp Add (Var "x") (Lit 1))) (BinOp Mul (Lit 3) (Lit 5))
