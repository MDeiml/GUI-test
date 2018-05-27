{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Rank2Types             #-}

module Renderer
    ( mainLoop
    , loadNinpatch
    , initState
    , renderStep
    , packWindow
    , Renderer(..)
    , RendererState
    , Globals(..)
    , App
    ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy      as BS
import           Data.IORef
import           Data.Text                 (Text)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           Drawable
import           GUI
import           Input
import           Resources
import           Types
import           Widget

type App t = Widget' t () ()

data RendererState t =
    RendererState (IORef (Resources t))
                  (App t)

initState :: App t -> IO (RendererState t)
initState app = do
    res <- newIORef resEmpty
    return $ RendererState res app

loadResource' ::
       (Renderer r t)
    => r
    -> IORef (Resources t)
    -> ResourceId a
    -> IO (Either String (a t))
loadResource' r ress i = do
    ress' <- readIORef ress
    case resLookup i ress' of
        Just res -> return $ Right res
        Nothing -> do
            res <- loadResource r i
            case res of
                Left s -> return $ Left s
                Right res' -> do
                    writeIORef ress $ resInsert i res' ress'
                    return res

mainLoop :: (Renderer r t) => r -> Int -> RendererState t -> IO ()
mainLoop r fps st = do
    st' <- fst <$> renderStep' frameTime r st
    c <- closing r
    unless c $ mainLoop r fps st'
  where
    frameTime = 1 / fromIntegral fps

renderStep :: (Renderer r t) => r -> RendererState t -> IO (RendererState t)
renderStep r st = fst <$> renderStep' 0 r st

packWindow :: (Renderer r t) => r -> RendererState t -> IO (RendererState t)
packWindow r st = do
    (st', p) <- renderStep' 0 r st
    let size = (ceiling $ pWidth p, ceiling $ pHeight p)
    setSize r size
    return st'

renderStep' ::
       (Renderer r t)
    => Double
    -> r
    -> RendererState t
    -> IO (RendererState t, LayoutParam)
renderStep' frameTime r (RendererState res w) = do
    events <- waitEvents r frameTime
    (width, height) <- getSize r
    let bs = Bounds 0 0 (fromIntegral width) (fromIntegral height)
    clear r
    now <- getPOSIXTime
    let glob =
            Globals
            { gEvents = events
            , gTime = round $ now * 1000
            , gResources = loadResource' r res
            }
        gui = runGUI $ runWidget w ()
    ~(~(_, p, ds'), ds) <- gui glob
    ~(w', ds'') <-
        runGUI
            (ds' [bs])
            glob
    forM_ (reverse ds ++ reverse ds'') $ \case
        Render d -> render r d
        RunIO m -> m
        _ -> return ()
    when
        (any
             (\case
                  StopTextInput -> True
                  _ -> False)
             ds) $
        stopTextInput r
    when
        (any
             (\case
                  StartTextInput -> True
                  _ -> False)
             ds) $
        startTextInput r
    swapBuffers r
    return (RendererState res w', fst $ head p)

loadNinpatch :: Renderer r t => r -> FilePath -> MaybeT IO (NinePatch t)
loadNinpatch r fp = do
    file <- lift $ BS.readFile fp
    res <- MaybeT $ return $ decode file
    (tf, xs, ys, xe, ye) <-
        MaybeT $
        return $
        flip parseMaybe res $ \obj -> do
            texFile <- obj .: "texture"
            xs' <- obj .: "xs"
            ys' <- obj .: "ys"
            xe' <- obj .: "xe"
            ye' <- obj .: "ye"
            return (texFile, xs', ys', xe', ye')
    Right tex@(Sprite _ w h) <- lift $ loadResource r $ ResS tf
    let f = fromIntegral
    return $ NP tex (f xs / f w, f ys / f h, f xe / f w, f ye / f h)

class Renderer r t | r -> t where
    create :: Text -> (Int, Int) -> IO r
    render :: r -> Drawable t -> IO ()
    clear :: r -> IO ()
    swapBuffers :: r -> IO ()
    getSize :: r -> IO (Int, Int)
    setSize :: r -> (Int, Int) -> IO ()
    closing :: r -> IO Bool
    waitEvents :: r -> Double -> IO [Event]
    loadResource :: r -> ResourceId a -> IO (Either String (a t))
    startTextInput :: r -> IO ()
    stopTextInput :: r -> IO ()
