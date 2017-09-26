module Widget
    ( Widget
    , runWidget
    , widgetOutput
    , buildWidget
    ) where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Types

data Widget g r i o = Widget
    { runWidget1 :: g -> [Bounds] -> i -> (o, [r], Widget g r i o)
    }

runWidget = runWidget1

widget :: (g -> [Bounds] -> i -> (o, [r], Widget g r i o)) -> Widget g r i o
widget r = Widget {runWidget1 = r}

buildWidget :: (g -> Bounds -> i -> (o, r, Widget g r i o)) -> Widget g r i o
buildWidget f = widget runWidget'
  where
    runWidget' g bs i = (o, [r], w')
      where
        (o, r, w') = f g (head bs) i

widgetOutput :: Widget g r (Bounds -> r) ()
widgetOutput = widget $ \_e bs r -> ((), [r $ head bs], widgetOutput)

instance Category (Widget g r) where
    id = widget runWidget'
      where
        runWidget' g bs i = (i, [], id)
    (.) a b = widget runWidget'
      where
        runWidget' g bs i = (oa, rb ++ ra, a' . b')
          where
            (ob, rb, b') = runWidget b g (take (length rb) bs) i
            (oa, ra, a') = runWidget a g (drop (length rb) bs) ob

instance Arrow (Widget g r) where
    arr f = widget runWidget'
      where
        runWidget' g bs i = (f i, [], arr f)
    (***) a b = widget runWidget'
      where
        runWidget' g bs (ia, ib) = ((oa, ob), ra ++ rb, a' *** b')
          where
            (oa, ra, a') = runWidget a g (take (length ra) bs) ia
            (ob, rb, b') = runWidget b g (drop (length ra) bs) ib

instance ArrowLoop (Widget g r) where
    loop a = widget runWidget'
      where
        runWidget' g bs i = (o, r, loop a')
          where
            ((o, s), r, a') = runWidget a g bs (i, s)

instance ArrowChoice (Widget g r) where
    (+++) a b = widget runWidget'
      where
        runWidget' g bs (Left i) = (Left o, r, a' +++ b)
          where
            (o, r, a') = runWidget a g bs i
        runWidget' g bs (Right i) = (Right o, r, a +++ b')
          where
            (o, r, b') = runWidget b g bs i
