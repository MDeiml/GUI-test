{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Rank2Types             #-}

module Renderer
    ( mainLoop
    , loadNinpatch
    , Renderer(..)
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

mainLoop :: (Renderer r t) => r -> Int -> App t -> IO ()
mainLoop r fps w0 = do
    res <- newIORef resEmpty
    mainLoop' w0 res
  where
    frameTime = 1 / fromIntegral fps
    mainLoop' w res = do
        events <- waitEvents r frameTime
        (width, height) <- getSize r
        clear r
        now <- getPOSIXTime
        let glob =
                Globals
                { gEvents = events
                , gTime = round $ now * 1000
                , gResources = loadResource' r res
                }
            gui = runGUI $ runWidget w ()
        ~(~(_, _, ds'), ds) <- gui glob
        ~(w', ds'') <- runGUI (ds' [Bounds 0 0 width height]) glob
        mapM_
            (\case
                 Render d -> render r d
                 RunIO m -> m
                 _ -> return ()) $
            reverse ds ++ reverse ds''
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
        c <- closing r
        unless c $ mainLoop' w' res

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
    getSize :: r -> IO (Float, Float)
    closing :: r -> IO Bool
    waitEvents :: r -> Double -> IO [Event]
    loadResource :: r -> ResourceId a -> IO (Either String (a t))
    startTextInput :: r -> IO ()
    stopTextInput :: r -> IO ()
