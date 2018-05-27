{-# LANGUAGE RecursiveDo #-}

module Widget
    ( Widget
    , runWidget
    , buildWidget'
    , buildWidget
    , buildWidgetStatic
    , buildWidgetStatic'
    , shift
    , widgetState
    ) where

import           Control.Arrow
import           Control.Category
import           Control.Monad.Fix
import           Prelude           hiding (id, (.))
import           Types

newtype Widget p m i o = Widget
    { runWidget1 :: i -> m (o, [p], [Bounds] -> m (Widget p m i o))
    }

runWidget = runWidget1

buildWidget ::
       Monad m
    => (i -> m (o, p, Bounds -> m (Widget p m i o)))
    -> Widget p m i o
buildWidget f = Widget runWidget'
  where
    runWidget' i = do
        (o, p, d) <- f i
        return (o, [p], d . head)

buildWidget' :: Monad m => (i -> m (o, Widget p m i o)) -> Widget p m i o
buildWidget' f =
    Widget $ \i -> do
        (o, w) <- f i
        return (o, [], const (return w))

buildWidgetStatic ::
       Monad m => (i -> m (o, p, Bounds -> m ())) -> Widget p m i o
buildWidgetStatic f = w
  where
    w =
        Widget $ \i -> do
            (o, p, d) <- f i
            return (o, [p], (>> return w) . d . head)

buildWidgetStatic' :: Monad m => (i -> m o) -> Widget p m i o
buildWidgetStatic' f = w
  where
    w =
        Widget $ \i -> do
            o <- f i
            return (o, [], const (return w))

shift :: MonadFix m => a -> Widget p m a a
shift x = buildWidget' $ \last -> return (x, shift last)

widgetState :: MonadFix m => s -> Widget p m (s, a) (s, b) -> Widget p m a b
widgetState s w = Widget runWidget'
  where
    runWidget' i = do
        ((s', o), p, d) <- runWidget w (s, i)
        return (o, p, fmap (widgetState s') . d)

instance Monad m => Category (Widget p m) where
    id = Widget (\i -> return (i, [], const (return id)))
    (.) a b = Widget runWidget'
      where
        runWidget' i = do
            (ob, pb, db) <- runWidget b i
            (oa, pa, da) <- runWidget a ob
            return
                ( oa
                , pb ++ pa
                , \bs -> do
                      b' <- db (take (length pb) bs)
                      a' <- da (drop (length pb) bs)
                      return (a' . b'))

instance Monad m => Arrow (Widget p m) where
    arr f = Widget $ \i -> return (f i, [], const $ return $ arr f)
    (***) a b = Widget runWidget'
      where
        runWidget' (ia, ib) = do
            (oa, pa, da) <- runWidget a ia
            (ob, pb, db) <- runWidget b ib
            return
                ( (oa, ob)
                , pa ++ pb
                , \bs -> do
                      a' <- da (take (length pa) bs)
                      b' <- db (drop (length pa) bs)
                      return $ a' *** b')

instance MonadFix m => ArrowLoop (Widget p m) where
    loop a = Widget runWidget'
      where
        runWidget' i = do
            rec ~(~(o, s), p, d) <- runWidget a (i, s)
            return (o, p, fmap loop . d)

instance MonadFix m => ArrowChoice (Widget p m) where
    left a = Widget runWidget'
      where
        runWidget' (Left i) = do
            (o, p, d) <- runWidget a i
            return (Left o, p, fmap left . d)
        runWidget' (Right i) = return (Right i, [], const (return $ left a))
