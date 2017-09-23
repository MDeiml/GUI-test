module Layout
  ( LayoutItem(..)
  , LayoutParam(..)
  , Orientation(..)
  , FillBehaviour(..)
  , Layout
  , widgetLayout
  , linearLayout
  , tableLayout
  , noLayout
  , stdParams
  ) where

import Data.List (transpose)
import Data.Maybe (catMaybes)
import Drawable
import Types
import Widget

data LayoutItem p = LI
  { layoutDrawables :: [Drawable]
  , layoutParam :: p
  } deriving (Show)

type Layout p1 p2 = [p1] -> (Bounds -> [Bounds], p2)

data Orientation
  = Horizontal
  | Vertical

data FillBehaviour
  = Fill
  | Wrap

data LayoutParam = LayoutParam
  { pWidth :: Float
  , pHeight :: Float
  , pWeightX :: Maybe Float
  , pWeightY :: Maybe Float
  } deriving (Show)

stdParams :: LayoutParam
stdParams =
  LayoutParam {pWidth = 0, pHeight = 0, pWeightX = Nothing, pWeightY = Nothing}

noLayout :: Layout p p
noLayout ps = (\bs -> bs : replicate (length ps - 1) (Bounds 0 0 0 0), head ps)

widgetLayout ::
     Layout p1 p2
  -> Widget g (LayoutItem p1) i o
  -> Widget g (LayoutItem p2) i o
widgetLayout f w = buildWidget runWidget'
  where
    runWidget' g bs i = (o, r', widgetLayout f w')
      where
        (o, r, w') = runWidget w g bs' i -- run widget (bounds are calculated later)
        r' = LI {layoutDrawables = drawables, layoutParam = p}
        (calcBounds, p) = f $ map layoutParam r -- f is evaluated without needing to know bounds
        bs' = calcBounds bs -- bounds are calculated from result of f
        drawables = concatMap layoutDrawables r

linearLayout ::
     Orientation
  -> (FillBehaviour, FillBehaviour)
  -> Layout LayoutParam LayoutParam
linearLayout o (lx, ly) ps = (calcBounds, param)
  where
    totalX = sum $ map pWidth ps
    totalY = sum $ map pHeight ps
    weightTotalX = sum $ catMaybes $ map pWeightX ps
    weightTotalY = sum $ catMaybes $ map pWeightY ps
    param =
      LayoutParam
      { pWidth = pWidth'
      , pHeight = pHeight'
      , pWeightX = pWeightX'
      , pWeightY = pWeightY'
      }
    pWidth' =
      case o of
        Vertical -> maximum $ map pWidth ps
        Horizontal -> totalX
    pHeight' =
      case o of
        Vertical -> maximum $ map pHeight ps
        Horizontal -> totalY
    pWeightX' =
      case lx of
        Fill -> Just 1
        Wrap -> Nothing
    pWeightY' =
      case ly of
        Fill -> Just 1
        Wrap -> Nothing
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
        stackH x ((Bounds a0 b0 a1 b1):bs) =
          (Bounds (a0 + x) b0 (a1 + x) b1) : stackH (a1 + x) bs
        stackV _ [] = []
        stackV y ((Bounds a0 b0 a1 b1):bs) =
          (Bounds a0 (b0 + y) a1 (b1 + y)) : stackV (b1 + y) bs

-- data TableLayoutParam = TableLayoutParam {
--     tpColspan :: Int,
--     tpLayoutParam :: LayoutParam
-- }
tableLayout ::
     Int -> (FillBehaviour, FillBehaviour) -> Layout LayoutParam LayoutParam
tableLayout colwidth (fx, fy) ps = (calcBounds, param)
  where
    ps' = reformat ps
    reformat [] = []
    reformat xs = take colwidth xs : reformat (drop colwidth xs)
    colWidths = map (maximum . map pWidth) $ transpose ps'
    colWeights = map (sum . catMaybes . map pWeightX) $ transpose ps'
    rowHeights = map (maximum . map pHeight) ps'
    rowWeights = map (sum . catMaybes . map pWeightY) ps'
    totalX = sum colWidths
    totalY = sum rowHeights
    totalWeightX = unzero $ sum colWeights
    totalWeightY = unzero $ sum rowWeights
    unzero 0 = 1
    unzero x = x
    param = stdParams {pWidth = totalX, pHeight = totalY}
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
          transpose $ zipWith (\w p -> map (fx w) p) colWidths' $ transpose ps'
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
        stackX (w:ws) t ((Bounds x0 y0 x1 y1):bs) =
          (Bounds (x0 + t) y0 (x1 + t) y1) : stackX ws (w + t) bs
        stackY _ _ [] = []
        stackY (h:hs) t ((Bounds x0 y0 x1 y1):bs) =
          (Bounds x0 (y0 + t) x1 (y1 + t)) : stackY hs (h + t) bs
