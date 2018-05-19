{-# LANGUAGE RecursiveDo #-}
module Widget
    ( Widget
    , runWidget
    , buildWidget'
    , buildWidget
    , shift
    ) where

import Control.Monad.Fix
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import GUI
import Types

newtype Widget p m i o = Widget
    { runWidget1 :: [Bounds] -> i -> m (o, [p], Widget p m i o)
    }

runWidget = runWidget1

widget ::
       MonadFix m => ([Bounds] -> i -> m (o, [p], Widget p m i o))
    -> Widget p m i o
widget = Widget

buildWidget ::
       MonadFix m => (Bounds -> i -> m (o, p, Widget p m i o)) -> Widget p m i o
buildWidget f = widget runWidget'
  where
    runWidget' bs i = do
        ~(o, p, w) <- f (head bs) i
        return (o, [p], w)

buildWidget' :: MonadFix m => (i -> m (o, Widget p m i o)) -> Widget p m i o
buildWidget' f =
    widget $ \_ i -> do
        (o, w) <- f i
        return (o, [], w)

shift :: MonadFix m => a -> Widget p m a a
shift x = widget $ \_ last -> return (x, [], shift last)

instance MonadFix m => Category (Widget p m) where
    id = widget $ \_ i -> return (i, [], id)
    (.) a b = widget runWidget'
      where
        runWidget' bs i = do
            rec ~(ob, pb, b') <- runWidget b (take (length pb) bs) i
            ~(oa, pa, a') <- runWidget a (drop (length pb) bs) ob
            return (oa, pb ++ pa, a' . b')

instance MonadFix m => Arrow (Widget p m) where
    arr f = widget $ \_ i -> return (f i, [], arr f)
    (***) a b = widget runWidget'
      where
        runWidget' bs ~(ia, ib) = do
            rec ~(oa, pa, a') <- runWidget a (take (length pa) bs) ia
            ~(ob, pb, b') <- runWidget b (drop (length pa) bs) ib
            return ((oa, ob), pa ++ pb, a' *** b')

instance MonadFix m => ArrowLoop (Widget p m) where
    loop a = widget runWidget'
      where
        runWidget' bs i = do
            rec ~(~(o, s), p, a') <- runWidget a bs (i, s)
            return (o, p, loop a')

instance MonadFix m => ArrowChoice (Widget p m) where
    left a = widget runWidget'
      where
        runWidget' bs (Left i) = do
            ~(o, p, w) <- runWidget a bs i
            return (Left o, p, left w)
        runWidget' bs (Right i) = return (Right i, [], left a)
