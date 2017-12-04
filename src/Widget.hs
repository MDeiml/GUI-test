{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Widget
    ( Widget
    , Vec(..)
    , Nat(..)
    , Nat'(..)
    , widgetOutput
    , widgetOutput'
    , buildWidget'
    , buildWidget
    , shift
    , widgetLayout
    , vmap
    , vHead
    , toList
    ) where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Types

data Nat
    = S Nat
    | Z

data Nat' (a :: Nat) where
    Z' :: Nat' Z
    S' :: Nat' n -> Nat' (S n)

dec :: Nat' (S n) -> Nat' n
dec (S' n) = n

data Vec :: Nat -> * -> * where
    Nil :: Vec Z a
    Cons :: a -> Vec n a -> Vec (S n) a

type family (x :: Nat) + (y :: Nat) where
    Z + y = y
    S x + y = S (x + y)

type family (x :: Nat) # (y :: Nat) where
    Z # y = Z
    S x # y = y + (x # y)

vmap :: (a -> b) -> Vec n a -> Vec n b
vmap _ Nil = Nil
vmap f (Cons x xs) = Cons (f x) (vmap f xs)

split :: Nat' n -> Nat' m -> Vec (n + m) c -> (Vec n c, Vec m c)
split Z' m cs = (Nil, cs)
split (S' n) m (Cons c cs) =
    let (a', b') = split n m cs
    in (Cons c a', b')

singleton :: a -> Vec (S Z) a
singleton x = Cons x Nil

vHead :: Vec (S n) a -> a
vHead (Cons x Nil) = x

vLength :: Vec n a -> Nat' n
vLength Nil = Z'
vLength (Cons x xs) = S' (vLength xs)

vconcat :: Vec n (Vec m a) -> Vec (n # m) a
vconcat Nil = Nil
vconcat (Cons x xs) = x `append` vconcat xs

vfill :: Nat' n -> Vec m a -> a -> Vec n a
vfill Z' _ _ = Nil
vfill (S' n) Nil x = Cons x (vfill n Nil x)
vfill (S' n) (Cons y ys) x = Cons y (vfill n ys x)

toList :: Vec n a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

append :: Vec n a -> Vec m a -> Vec (n + m) a
append Nil ys = ys
append (Cons x xs) ys = Cons x (xs `append` ys)

type W g r s p i o
     = g -> Vec s Bounds -> i -> (o, Vec s p, [r], Widget g r p i o)

data Widget g r p i o where
    Widget :: W g r s p i o -> Widget g r p i o

withWidgets ::
       Widget g r p i o
    -> Widget g2 r2 p2 i2 o2
    -> (forall s s2. W g r s p i o -> forall s. W g2 r2 s2 p2 i2 o2 -> W g1 r1 s1 p1 i1 o1)
    -> Widget g1 r1 p1 i1 o1
withWidgets (Widget r1) (Widget r2) f = Widget $ f r1 r2

widget ::
       (g -> Vec s Bounds -> i -> (o, Vec s p, [r], Widget g r p i o))
    -> Widget g r p i o
widget = Widget

widgetLayout ::
       (forall s. Vec s p -> (Bounds -> Vec s Bounds, p1))
    -> Widget g r p i o
    -> Widget g r p1 i o
widgetLayout f (Widget a) = Widget runWidget'
  where
    runWidget' g b i = (o, singleton p, rs, widgetLayout f a')
      where
        (o, ps, rs, a') = a g (cb $ vHead b) i
        (cb, p) = f ps

buildWidget ::
       (g -> Bounds -> i -> (o, p, [r], Widget g r p i o)) -> Widget g r p i o
buildWidget f = widget runWidget'
  where
    runWidget' g bs i = (o, singleton p, r, w)
      where
        ~(o, p, r, w) = f g (vHead bs) i

buildWidget' :: (g -> i -> (o, Widget g r p i o)) -> Widget g r p i o
buildWidget' f =
    widget $ \g _ i ->
        let (o, w) = f g i
        in (o, Nil, [], w)

widgetOutput :: Widget g r p (p, Bounds -> [r]) ()
widgetOutput =
    widget $ \_ bs (p, r) -> ((), singleton p, r $ vHead bs, widgetOutput)

widgetOutput' :: Widget g r p [r] ()
widgetOutput' = widget $ \_ _ r -> ((), Nil, r, widgetOutput')

shift :: a -> Widget g r p a a
shift x = widget $ \_ _ last -> (x, Nil, [], shift last)

instance Category (Widget g r p) where
    id = widget $ \_ bs i -> (i, Nil, [], id)
    (.) (Widget a) (Widget b) = Widget runWidget1
      where
        runWidget1 g bs i = (oa, pb `append` pa, rb ++ ra, a' . b')
          where
            ~(bb, ba) = split (vLength pb) (vLength pa) bs
            ~(ob, pb, rb, b') = b g bb i
            ~(oa, pa, ra, a') = a g ba ob

instance Arrow (Widget g r p) where
    arr f = widget $ \_ _ i -> (f i, Nil, [], arr f)
    (***) (Widget a) (Widget b) = Widget runWidget'
      where
        runWidget' g bs ~(ia, ib) =
            ((oa, ob), pa `append` pb, ra ++ rb, a' *** b')
          where
            ~(ba, bb) = split (vLength pa) (vLength pb) bs
            ~(oa, pa, ra, a') = a g ba ia
            ~(ob, pb, rb, b') = b g bb ib

instance ArrowLoop (Widget g r p) where
    loop (Widget a) = Widget runWidget'
      where
        runWidget' g bs i = (o, p, r, loop a')
          where
            ~(~(o, s), p, r, a') = a g bs (i, s)
