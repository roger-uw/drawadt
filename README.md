# drawadt

## A Haskell library for data type visualisation

### Example: Draw a list

```haskell
import Data.Text.DrawADT

-- class Draw (repr :: * -> *) where
--   line :: a -> String -> repr a
--   (++>) :: repr (a -> b) -> repr a -> repr b
--   (<++) :: repr a -> repr (a -> b) -> repr b

-- class DrawADT repr a where
--   draw :: a -> repr a

instance (Draw repr, Show a) => DrawADT repr [a] where
-- draw :: (Draw repr, Show a) => [a] -> repr [a]
  draw = foldr f (line [] " ~")
    where f now acc = line (now :) (show now) ++> acc
```

This library is developed in the [finally tagless](http://okmij.org/ftp/tagless-final/JFP.pdf) style [[1](#1), [2](#2)], so the interpretations of `line`, `++>` and `<++` are decided by the instance of `Draw`. In the following examples, `line a s` creates a single line of text representing `a` with `s`, `a ++> b` connects `b` to the right side of `a`, and `b <++ a` connects `b` to the left side of `a`.

In this library, left and right on the screen are defined as follows.

```
Left
  ↑
  -
  ↓
Right
```

For the empty list, we create a line containing a string `" ~"`. For each element in the list, we create a line for it and connect it to the drawn list.

```haskell
testList :: [Int]
testList = [1..5]
```

Now we select the Unicode render and apply the default configuration. The following auxiliary functions will also be used for other examples.

```haskell
toText :: RenderUnicodeDraw VL.Vec [] T.Text a -> T.Text
toText = runRenderUnicodeDraw defaultUnicodeConfig

printDraw :: RenderUnicodeDraw VL.Vec [] T.Text a -> IO ()
printDraw = T.putStr . toText
```

Run `printDraw (draw testList)` and we will get:

_may not be correctly displayed if the font is not monospaced_

```
────1
    └────2
         └────3
              └────4
                   └────5
                        └──── ~
```

### Example: Draw a binary tree

With [recursion schemes](http://hackage.haskell.org/package/recursion-schemes) [[3](#3)], we can implement the `DrawADT` instances for our data types in a more elegant way.

```haskell
data BinTree a = BinEmpty | BinTree a (BinTree a) (BinTree a) deriving (Show)

makeBaseFunctor ''BinTree
```

We can define the `draw` function with `cata`, and generate trees using `ana`.

```haskell
instance (Draw repr, Show a) => DrawADT repr (BinTree a) where
  draw = cata alg
    where alg (BinTreeF a l r) = l <++ line (BinTree a) (show a) ++> r
          alg BinEmptyF = line BinEmpty " ~"

testBinTree :: Int -> BinTree Int
testBinTree = ana coalg
  where coalg n
          | n <= 0 = BinEmptyF
          | otherwise = BinTreeF n (n - 1) (n - 2)
```

Run `printDraw (draw (testBinTree 4))` and we will get:

_may not be correctly displayed if the font is not monospaced_

```
                   ┌──── ~
              ┌────1
              │    └──── ~
         ┌────2
         │    └──── ~
    ┌────3
    │    │    ┌──── ~
    │    └────1
    │         └──── ~
────4
    │         ┌──── ~
    │    ┌────1
    │    │    └──── ~
    └────2
         └──── ~
```

If we do not want to see these `~` for empty trees, the following code presents a possible solution to eliminate them.

```haskell
newtype LeafBinTree a = LeafBinTree (BinTree a)

-- apply is defined in Extend.hs
instance (Draw repr, ApplyDraw repr, Show a) => DrawADT repr (LeafBinTree a) where
  draw (LeafBinTree t) = apply LeafBinTree (para alg t)
    where alg (BinTreeF a (BinEmpty, _) (BinEmpty, _)) = line (BinTree a BinEmpty BinEmpty) (show a)
          alg (BinTreeF a (_, l) (BinEmpty, _)) = l <++ line (flip (BinTree a) BinEmpty) (show a)
          alg (BinTreeF a (BinEmpty, _) (_, r)) = line (BinTree a BinEmpty) (show a) ++> r
          alg (BinTreeF a (_, l) (_, r)) = l <++ line (BinTree a) (show a) ++> r
```

Here we use `para` to see the sub-structures.

Run `printDraw (draw (LeafBinTree (testBinTree 4)))` and we will get:

_may not be correctly displayed if the font is not monospaced_

```
              ┌────1
         ┌────2
    ┌────3
    │    └────1
────4
    │    ┌────1
    └────2
```

A code snippet by Donnacha Oisín Kidney for a similar purpose can be found at [Drawing Trees](https://doisinkidney.com/snippets/drawing-trees.html).

### Example: Draw a quadtree

```haskell
data QuadTree a = QuadEmpty | QuadTree a (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a) deriving (Show)

makeBaseFunctor ''QuadTree
```

This `DrawADT` instance for `QuadTree` is relatively straightforward. A noteworthy point is the consecutive use of the `++>` and `<++` operators.

```haskell
instance (Draw repr, Show a) => DrawADT repr (QuadTree a) where
  draw = cata alg
    where alg (QuadTreeF a fl nl nr fr) = fl <++ nl <++ line (QuadTree a) (show a) ++> nr ++> fr
          alg QuadEmptyF = line QuadEmpty " ~"

testQuadTree :: Int -> QuadTree Int
testQuadTree = ana coalg
  where coalg n
          | n <= 0 = QuadEmptyF
          | otherwise = QuadTreeF n (n - 1) (n - 2) (n - 3) (n - 4)
```

Run `printDraw (draw (testQuadTree 3))` and we will get:

_may not be correctly displayed if the font is not monospaced_

```
              ┌──── ~
              ├──── ~
         ┌────1
         │    ├──── ~
         │    └──── ~
         ├──── ~
    ┌────2
    │    ├──── ~
    │    └──── ~
    │    ┌──── ~
    │    ├──── ~
    ├────1
    │    ├──── ~
    │    └──── ~
────3
    ├──── ~
    └──── ~
```

We can define a new instance for `QuadTree` to remove these `~` as in the `BinTree` example, but let's concentrate on another potential problem in current implementation. If the data held by the `QuadTree` is not simply integers, but more complex data types, such as lists or even quadtrees, we may need to crush a complex structure into one line or add an extra branch to display the data. Is there a better solution?

Yes. By including `BlockDraw repr` in the constraints, we can hold the data in an anonymous `block` and use the block as a _centre_ node. The `block` is called anonymous because it does not take a `String` as a parameter like `line`.

_A named block may introduce extra complexity, so it is not provided in this library._

```haskell
newtype ComplexQuadTree a = ComplexQuadTree (QuadTree a)

-- class BlockDraw (repr :: * -> *) where
--   block :: (a -> b) -> repr a -> repr b
instance (Draw repr, BlockDraw repr, ApplyDraw repr, DrawADT repr a) => DrawADT repr (ComplexQuadTree a) where
  draw (ComplexQuadTree t) = apply ComplexQuadTree (cata alg t)
    where alg (QuadTreeF a fl nl nr fr) = fl <++ nl <++ block QuadTree (draw a) ++> nr ++> fr
          alg QuadEmptyF = line QuadEmpty " ~"

testComplexQuadTree :: Int -> QuadTree [Int]
testComplexQuadTree = ana coalg
  where coalg n
          | n <= 0 = QuadEmptyF
          | otherwise = QuadTreeF [0..n] (n - 1) (n - 2) (n - 3) (n - 4)
```

Run `printDraw (draw (ComplexQuadTree (testComplexQuadTree 3)))` and we will get:

```
              ┌──── ~
              ├──── ~
         ┌────╬────0
         │    │    └────1
         │    │         └──── ~
         │    ├──── ~
         │    └──── ~
         ├──── ~
    ┌────╬────0
    │    │    └────1
    │    │         └────2
    │    │              └──── ~
    │    ├──── ~
    │    └──── ~
    │    ┌──── ~
    │    ├──── ~
    ├────╬────0
    │    │    └────1
    │    │         └──── ~
    │    ├──── ~
    │    └──── ~
────╬────0
    │    └────1
    │         └────2
    │              └────3
    │                   └──── ~
    ├──── ~
    └──── ~
```

##### [1]
Oleg Kiselyov. 2010. Typed tagless final interpreters. In Proceedings of the 2010 international spring school conference on Generic and Indexed Programming (SSGIP'10), Jeremy Gibbons (Ed.). Springer-Verlag, Berlin, Heidelberg, 130-174.

##### [2]
Jacques Carette, Oleg Kiselyov, and Chung-chieh Shan. 2009. Finally tagless, partially evaluated: Tagless staged interpreters for simpler typed languages. J. Funct. Program. 19, 5 (September 2009), 509-543.

##### [3]
Erik Meijer, Maarten Fokkinga, and Ross Paterson. 1991. Functional programming with bananas, lenses, envelopes and barbed wire. In Proceedings of the 5th ACM conference on Functional programming languages and computer architecture, J. Hughes (Ed.). Springer-Verlag, Berlin, Heidelberg, 124-144.