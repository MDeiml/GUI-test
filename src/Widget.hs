module Widget
    ( Widget
    , runWidget
    , widgetOutput
    , buildWidget'
    , buildWidget
    , shift
    ) where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Types

newtype Widget g r p i o = Widget
    { runWidget1 :: g -> [Bounds] -> i -> (o, [p], [r], Widget g r p i o)
    }

runWidget = runWidget1

widget ::
       (g -> [Bounds] -> i -> (o, [p], [r], Widget g r p i o))
    -> Widget g r p i o
widget = Widget

buildWidget ::
       (g -> Bounds -> i -> (o, p, r, Widget g r p i o)) -> Widget g r p i o
buildWidget f = widget runWidget'
  where
    runWidget' g bs i = (o, [p], [r], w)
      where
        ~(o, p, r, w) = f g (head bs) i

buildWidget' :: (g -> i -> (o, Widget g r p i o)) -> Widget g r p i o
buildWidget' f =
    widget $ \g _ i ->
        let (o, w) = f g i
        in (o, [], [], w)

widgetOutput :: Widget g r p (p, Bounds -> r) ()
widgetOutput = widget $ \_ bs (p, r) -> ((), [p], [r $ head bs], widgetOutput)

shift :: a -> Widget g r p a a
shift x = widget $ \_ _ last -> (x, [], [], shift last)

instance Category (Widget g r p) where
    id = widget $ \_ _ i -> (i, [], [], id)
    (.) a b = widget runWidget'
      where
        runWidget' g bs i = (oa, pb ++ pa, rb ++ ra, a' . b')
          where
            ~(ob, pb, rb, b') = runWidget b g (take (length pb) bs) i
            ~(oa, pa, ra, a') = runWidget a g (drop (length pb) bs) ob

instance Arrow (Widget g r p) where
    arr f = widget $ \_ _ i -> (f i, [], [], arr f)
    (***) a b = widget runWidget'
      where
        runWidget' g bs ~(ia, ib) = ((oa, ob), pa ++ pb, ra ++ rb, a' *** b')
          where
            ~(oa, pa, ra, a') = runWidget a g (take (length pa) bs) ia
            ~(ob, pb, rb, b') = runWidget b g (drop (length pa) bs) ib

instance ArrowLoop (Widget g r p) where
    loop a = widget runWidget'
      where
        runWidget' g bs i = (o, p, r, loop a')
          where
            ~(~(o, s), p, r, a') = runWidget a g bs (i, s)
