{-# LANGUAGE RecursiveDo #-}
module Widget
    ( Widget
    , runWidget
    , widgetOutput
    , widgetOutput'
    , buildWidget'
    , buildWidget
    , shift
    ) where

import Control.Monad.Fix
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Types

newtype Widget g r p m i o = Widget
    { runWidget1 :: g -> [Bounds] -> i -> m (o, [p], [r], Widget g r p m i o)
    }

runWidget = runWidget1

widget ::
       MonadFix m => (g -> [Bounds] -> i -> m (o, [p], [r], Widget g r p m i o))
    -> Widget g r p m i o
widget = Widget

buildWidget ::
       MonadFix m => (g -> Bounds -> i -> m (o, p, [r], Widget g r p m i o)) -> Widget g r p m i o
buildWidget f = widget runWidget'
  where
    runWidget' g bs i = do
        ~(o, p, r, w) <- f g (head bs) i
        return (o, [p], r, w)

buildWidget' :: MonadFix m => (g -> i -> m (o, Widget g r p m i o)) -> Widget g r p m i o
buildWidget' f =
    widget $ \g _ i -> do
        (o, w) <- f g i
        return (o, [], [], w)

widgetOutput :: MonadFix m => Widget g r p m (p, Bounds -> [r]) ()
widgetOutput = widget $ \_ bs (p, r) -> return ((), [p], r $ head bs, widgetOutput)

widgetOutput' :: MonadFix m => Widget g r p m [r] ()
widgetOutput' = widget $ \_ _ r -> return ((), [], r, widgetOutput')

shift :: MonadFix m => a -> Widget g r p m a a
shift x = widget $ \_ _ last -> return (x, [], [], shift last)

instance MonadFix m => Category (Widget g r p m) where
    id = widget $ \_ _ i -> return (i, [], [], id)
    (.) a b = widget runWidget'
      where
        runWidget' g bs i = do
            rec ~(ob, pb, rb, b') <- runWidget b g (take (length pb) bs) i
            ~(oa, pa, ra, a') <- runWidget a g (drop (length pb) bs) ob
            return (oa, pb ++ pa, rb ++ ra, a' . b')

instance MonadFix m => Arrow (Widget g r p m) where
    arr f = widget $ \_ _ i -> return (f i, [], [], arr f)
    (***) a b = widget runWidget'
      where
        runWidget' g bs ~(ia, ib) = do
            rec ~(oa, pa, ra, a') <- runWidget a g (take (length pa) bs) ia
            ~(ob, pb, rb, b') <- runWidget b g (drop (length pa) bs) ib
            return ((oa, ob), pa ++ pb, ra ++ rb, a' *** b')

instance MonadFix m => ArrowLoop (Widget g r p m) where
    loop a = widget runWidget'
      where
        runWidget' g bs i = do
            rec ~(~(o, s), p, r, a') <- runWidget a g bs (i, s)
            return (o, p, r, loop a')

instance MonadFix m => ArrowChoice (Widget g r p m) where
    left a = widget runWidget'
      where
        runWidget' g bs (Left i) = do
            (o, p, r, w) <- runWidget a g bs i
            return (Left o, p, r, left w)
        runWidget' g bs (Right i) = return (Right i, [], [], left a)
