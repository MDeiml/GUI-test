module Widget
    ( Widget
    , runWidget
    , widgetOutput
    , widgetOutput'
    , buildWidget
    , shift
    ) where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Types

newtype Widget g r p i o = Widget
    { runWidget1 :: g -> ( [p]
                         , i -> [Bounds] -> (o, [[r]], Bool, Widget g r p i o))
    }

runWidget = runWidget1

widget ::
       (g -> ([p], i -> [Bounds] -> (o, [[r]], Bool, Widget g r p i o)))
    -> Widget g r p i o
widget r = Widget {runWidget1 = r}

buildWidget ::
       (g -> (p, i -> Bounds -> (o, [r], Bool, Widget g r p i o)))
    -> Widget g r p i o
buildWidget f = widget runWidget'
  where
    runWidget' g = ([p], runWidget'')
      where
        (p, f') = f g
        runWidget'' i bs = (o, [r], d, w)
          where
            (o, r, d, w) = f' i $ head bs

widgetOutput :: p -> Widget g r p (Bounds -> [r]) ()
widgetOutput p =
    widget $ const ([p], \r bs -> ((), [r $ head bs], True, widgetOutput p))

widgetOutput' :: p -> Widget g r p (Bounds -> [r], Bool) ()
widgetOutput' p =
    widget $ const ([p], \(r, d) bs -> ((), [r $ head bs], d, widgetOutput' p))

shift :: a -> Widget g r p a a
shift initial = widget $ const ([], \last _ -> (initial, [], False, shift last))

instance Category (Widget g r p) where
    id = widget $ const ([], \i _ -> (i, [], False, id))
    (.) a b = widget runWidget'
      where
        runWidget' g = (pb ++ pa, runWidget'')
          where
            (pb, b') = runWidget b g
            (pa, a') = runWidget a g
            runWidget'' i bs = (oa, rb ++ ra, db || da, a'' . b'')
              where
                (ob, rb, db, b'') = b' i $ take (length pb) bs
                (oa, ra, da, a'') = a' ob $ drop (length pb) bs

instance Arrow (Widget g r p) where
    arr f = widget $ const ([], \i _ -> (f i, [], False, arr f))
    (***) a b = widget runWidget'
      where
        runWidget' g = (pa ++ pb, runWidget'')
          where
            (pa, a') = runWidget a g
            (pb, b') = runWidget b g
            runWidget'' (ia, ib) bs =
                ((oa, ob), ra ++ rb, da || db, a'' *** b'')
              where
                (oa, ra, da, a'') = a' ia bs
                (ob, rb, db, b'') = b' ib bs

instance ArrowLoop (Widget g r p) where
    loop a = widget runWidget'
      where
        runWidget' g = (p, runWidget'')
          where
            (p, a') = runWidget a g
            runWidget'' i bs = (o, r, d, loop a'')
              where
                ((o, s), r, d, a'') = a' (i, s) bs
