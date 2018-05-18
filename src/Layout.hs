{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
module Layout
    ( LayoutParam(..)
    , Orientation(..)
    , Align(..)
    , Layout
    , Weight
    , widgetLayout
    , linearLayout
    , tableLayout
    , widgetLayout'
    , linearLayout'
    , tableLayout'
    , noLayout
    , stackLayout
    , stdParams
    ) where

import Data.List (transpose)
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Drawable
import Types
import Widget
import Control.Monad.Fix

type Layout1 p1 p2 = [p1] -> (Bounds -> [Bounds], p2)

type Layout' p1 p2 r a = forall g m i o. MonadFix m => Widget g r (p1, Bool) m i (a, o) -> Widget g r (p2, Bool) m i o

type Layout p1 p2 d = forall g m i o. MonadFix m => Widget g d (p1, Bool) m i o -> Widget g d (p2, Bool) m i o

type Weight = Maybe Float

type Margin = (Float, Float, Float, Float)

data Orientation
    = Horizontal
    | Vertical

data Align
    = AlignLeft
    | AlignCenter
    | AlignRight

data LayoutParam = LayoutParam
    { pWidth :: Float
    , pHeight :: Float
    , pWeightX :: Weight
    , pWeightY :: Weight
    } deriving (Show, Eq)

stdParams :: LayoutParam
stdParams =
    LayoutParam
    {pWidth = 0, pHeight = 0, pWeightX = Nothing, pWeightY = Nothing}

noLayout :: (Eq p) => Layout p p d
noLayout =
    widgetLayout $ \ps ->
        (\bs -> bs : replicate (length ps - 1) (Bounds 0 0 0 0), head ps)

widgetLayout' ::
       (a -> Layout1 p1 p2) -> Layout' p1 p2 r a
widgetLayout' f a = buildWidget runWidget'
  where
    runWidget' g bs i = do
        rec ((oa, o), ps, rs, a') <- runWidget a g bs' i
            let (calcBounds, p) = f oa $ map (\ ~(x,_) -> x) ps
                bs' = calcBounds bs
        return (o, (p, True), rs, widgetLayout' f a')

widgetLayout :: (Eq p1) => Layout1 p1 p2 -> Layout p1 p2 d 
widgetLayout f w = buildWidget $ runWidget' w Nothing
  where
      runWidget' w0 mbs g bs i = do
        rec ~(o, ps0, rs, w') <- runWidget w0 g bs' i
            let ps = map (\ ~(x,_) -> x) ps0
                reval = any (\ ~(_,x) -> x) ps0
                ~(~(calcBounds, p), reval') = 
                    case mbs of
                        Nothing -> (f ps, True)
                        Just ~(ps', p', cb, b, bs) ->
                            if not reval
                                then (( \x ->
                                           if x == b
                                               then bs
                                               else cb x
                                      , p'), False)
                                else (f ps, True)
                bs' = calcBounds bs
        return ( o
            , (p, reval')
            , rs
            , buildWidget $ runWidget' w' $ Just (ps, p, calcBounds, bs, bs')
             )

stackLayout ::
       Margin
    -> (Align, Align)
    -> (Weight, Weight)
    -> Layout LayoutParam LayoutParam d 
stackLayout m a l = widgetLayout $ stackLayout1 m a l

stackLayout' ::
       Layout' LayoutParam LayoutParam d ( Margin
                                               , (Align, Align)
                                               , (Weight, Weight))
stackLayout' = widgetLayout' $ \(a, b, c) -> stackLayout1 a b c

stackLayout1 ::
       Margin
    -> (Align, Align)
    -> (Weight, Weight)
    -> Layout1 LayoutParam LayoutParam
stackLayout1 (mt, mb, mr, ml) (ax, ay) (lx, ly) ps =
    (\bs -> map (calcBounds bs) ps, param)
  where
    pw = mr + ml + pWidth (head ps)
    ph = mt + mb + pHeight (head ps)
    param =
        LayoutParam {pWidth = pw, pHeight = ph, pWeightX = lx, pWeightY = ly}
    calcBounds (Bounds x0' y0' x1' y1') p = bs
      where
        x0 = x0' + ml
        y0 = y0' + mt
        x1 = x1' - mr
        y1 = y1' - mb
        bs = Bounds (xs + x0) (ys + y0) (xs + x0 + width) (ys + y0 + height)
        width =
            case pWeightX p of
                Nothing -> pWidth (head ps)
                Just _ -> x1 - x0
        height =
            case pWeightY p of
                Nothing -> pHeight (head ps)
                Just _ -> y1 - y0
        xs =
            case ax of
                AlignLeft -> 0
                AlignCenter -> ((x1 - x0) - width) / 2
                AlignRight -> (x1 - x0) - width
        ys =
            case ay of
                AlignLeft -> 0
                AlignCenter -> ((y1 - y0) - height) / 2
                AlignRight -> (y1 - y0) - height

linearLayout ::
       Orientation -> (Weight, Weight) -> Layout LayoutParam LayoutParam d
linearLayout o l = widgetLayout $ linearLayout1 o l

linearLayout' ::
       Layout' LayoutParam LayoutParam d (Orientation, (Weight, Weight))
linearLayout' = widgetLayout' $ uncurry linearLayout1

linearLayout1 ::
       Orientation -> (Weight, Weight) -> Layout1 LayoutParam LayoutParam
linearLayout1 o (lx, ly) ps = (calcBounds, param)
  where
    totalX = sum $ map pWidth ps
    totalY = sum $ map pHeight ps
    weightTotalX = sum $ mapMaybe pWeightX ps
    weightTotalY = sum $ mapMaybe pWeightY ps
    param =
        LayoutParam
        {pWidth = pWidth', pHeight = pHeight', pWeightX = lx, pWeightY = ly}
    pWidth' =
        case o of
            Vertical -> maximum $ map pWidth ps
            Horizontal -> totalX
    pHeight' =
        case o of
            Vertical -> maximum $ map pHeight ps
            Horizontal -> totalY
    calcBounds (Bounds x0 y0 x1 y1) =
        case o of
            Horizontal -> stackH x0 $ map calcBoundsH ps
            Vertical -> stackV y0 $ map calcBoundsV ps
      where
        restX = (x1 - x0) - totalX
        restY = (y1 - y0) - totalY
        weightX = maybe 0 (\w -> restX / weightTotalX * w)
        weightY = maybe 0 (\w -> restY / weightTotalY * w)
        calcBoundsH p =
            Bounds
                0
                y0
                (pWidth p + weightX (pWeightX p))
                (maybe (pHeight p) (const $ y1 - y0) $ pWeightY p)
        calcBoundsV p =
            Bounds
                x0
                0
                (maybe (pWidth p) (const $ x1 - x0) $ pWeightX p)
                (pHeight p + weightY (pWeightY p))
        stackH _ [] = []
        stackH x (Bounds a0 b0 a1 b1:bs) =
            Bounds (a0 + x) b0 (a1 + x) b1 : stackH (a1 + x) bs
        stackV _ [] = []
        stackV y (Bounds a0 b0 a1 b1:bs) =
            Bounds a0 (b0 + y) a1 (b1 + y) : stackV (b1 + y) bs

-- data TableLayoutParam = TableLayoutParam {
--     tpColspan :: Int,
--     tpLayoutParam :: LayoutParam
-- }
tableLayout :: Int -> (Weight, Weight) -> Layout LayoutParam LayoutParam d 
tableLayout colwidth l = widgetLayout $ tableLayout1 colwidth l

tableLayout' :: Layout' LayoutParam LayoutParam d (Int, (Weight, Weight))
tableLayout' = widgetLayout' $ uncurry tableLayout1

tableLayout1 :: Int -> (Weight, Weight) -> Layout1 LayoutParam LayoutParam
tableLayout1 colwidth (lx, ly) ps = (calcBounds, param)
  where
    ps' = reformat ps
    reformat [] = []
    reformat xs = take colwidth xs : reformat (drop colwidth xs)
    colWidths = map (maximum . map pWidth) $ transpose ps'
    colWeights = map (sum . mapMaybe pWeightX) $ transpose ps'
    rowHeights = map (maximum . map pHeight) ps'
    rowWeights = map (sum . mapMaybe pWeightY) ps'
    totalX = sum colWidths
    totalY = sum rowHeights
    totalWeightX = unzero $ sum colWeights
    totalWeightY = unzero $ sum rowWeights
    unzero 0 = 1
    unzero x = x
    param =
        LayoutParam
        {pWidth = totalX, pHeight = totalY, pWeightX = lx, pWeightY = ly}
    calcBounds (Bounds x0 y0 x1 y1) = concat bounds'
      where
        restX = (x1 - x0) - totalX
        restY = (y1 - y0) - totalY
        colWidths' =
            zipWith (+) colWidths $
            map (\w -> restX / totalWeightX * w) colWeights
        rowHeights' =
            zipWith (+) rowHeights $
            map (\w -> restY / totalWeightY * w) rowWeights
        widths =
            transpose $
            zipWith (\w p -> map (fx w) p) colWidths' $ transpose ps'
        heights = zipWith (\h p -> map (fy h) p) rowHeights' ps'
        fx w p =
            case pWeightX p of
                Nothing -> pWidth p
                Just _ -> w
        fy h p =
            case pWeightY p of
                Nothing -> pHeight p
                Just _ -> h
        bounds = zipWith (zipWith (Bounds 0 0)) widths heights
        bounds' =
            transpose $
            map (stackY rowHeights' y0) $
            transpose $ map (stackX colWidths' x0) bounds
        stackX _ _ [] = []
        stackX (w:ws) t (Bounds x0 y0 x1 y1:bs) =
            Bounds (x0 + t) y0 (x1 + t) y1 : stackX ws (w + t) bs
        stackY _ _ [] = []
        stackY (h:hs) t (Bounds x0 y0 x1 y1:bs) =
            Bounds x0 (y0 + t) x1 (y1 + t) : stackY hs (h + t) bs
