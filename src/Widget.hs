module Widget
    ( Widget
    , runWidget
    , widgetOutput
    , buildWidget
    , shift
    ) where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Types

newtype Widget g r p i o = Widget
    { runWidget1 :: g -> ([p], i -> [Bounds] -> (o, [r], Widget g r p i o))
    }

runWidget = runWidget1

widget ::
       (g -> ([p], i -> [Bounds] -> (o, [r], Widget g r p i o)))
    -> Widget g r p i o
widget r = Widget {runWidget1 = r}

buildWidget ::
       (g -> (p, i -> Bounds -> (o, r, Widget g r p i o))) -> Widget g r p i o
buildWidget f = widget runWidget'
  where
    runWidget' g = ([p], runWidget'')
      where
        (p, f') = f g
        runWidget'' i bs = (o, [r], w)
          where
            (o, r, w) = f' i $ head bs

widgetOutput :: p -> Widget g r p (Bounds -> r) ()
widgetOutput p =
    widget $ const ([p], \r bs -> ((), [r $ head bs], widgetOutput p))

shift :: a -> Widget g r p a a
shift initial = widget $ const ([], \last _ -> (initial, [], shift last))

instance Category (Widget g r p) where
    id = widget $ const ([], \i _ -> (i, [], id))
    (.) a b = widget runWidget'
      where
        runWidget' g = (pb ++ pa, runWidget'')
          where
            (pb, b') = runWidget b g
            (pa, a') = runWidget a g
            runWidget'' i bs = (oa, rb ++ ra, a'' . b'')
              where
                (ob, rb, b'') = b' i $ take (length pb) bs
                (oa, ra, a'') = a' ob $ drop (length pb) bs

instance Arrow (Widget g r p) where
    arr f = widget $ const ([], \i _ -> (f i, [], arr f))
    (***) a b = widget runWidget'
      where
        runWidget' g = (pa ++ pb, runWidget'')
          where
            (pa, a') = runWidget a g
            (pb, b') = runWidget b g
            runWidget'' (ia, ib) bs = ((oa, ob), ra ++ rb, a'' *** b'')
              where
                (oa, ra, a'') = a' ia bs
                (ob, rb, b'') = b' ib bs

instance ArrowLoop (Widget g r p) where
    loop a = widget runWidget'
      where
        runWidget' g = (p, runWidget'')
          where
            (p, a') = runWidget a g
            runWidget'' i bs = (o, r, loop a'')
              where
                ((o, s), r, a'') = a' (i, s) bs
