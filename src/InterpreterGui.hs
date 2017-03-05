module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Control.Monad
import Control.Monad.Trans(liftIO)
import Interpreter
import Data.IORef
import Data.Bits
import Data.Int
import Data.Word
import Data.Map(Map)
import Data.Ix(inRange)
import qualified Data.Map as Map
import qualified Data.Array.MArray as A
import Debug.Trace

drawFB :: Pixbuf -> Mem -> IO ()
drawFB pb fb = do
    pxls <- pixbufGetPixels pb :: IO (PixbufData Int Word8)
    rs <- pixbufGetRowstride pb
    let drawPxl :: Int -> Int -> Int16 -> IO ()
        drawPxl x y px | inRange (0,639) x && inRange (0,199) y = do
            A.writeArray pxls (3*x+y*rs) (fromIntegral $ px .&. 0xe0)
            A.writeArray pxls (3*x+1+y*rs) (fromIntegral $ (px .&. 0x1c) `shiftL` 3)
            A.writeArray pxls (3*x+2+y*rs) (fromIntegral $ (px .&. 0x3) `shiftL` 6)
                       | otherwise = return ()
    forM_ (Map.toAscList fb) $ \(i,w) -> do
        let i' = i .&. 0x7fff
        let xs = 160*((i `shiftR` 15) .&. 1)
        let (x,y) = (fromIntegral xs + fromIntegral i' `mod` 160, fromIntegral i' `div` 160)
        let (p1, p2) = ((w `shiftR` 8) .&. 0xff, w .&. 0xff)
        drawPxl (2*x) y p2
        drawPxl (2*x+1) y p1

loadProgram :: String -> Mem
loadProgram = Map.fromAscList . zip [0..] . map (read . ("0x"++)) . words

initFB :: Mem
initFB = Map.fromList [(161,0x00ff),(322,0xff00),(481,0xffff),(482,0xff00)]

main = do
    i <- getContents
    let prog = loadProgram i
    initGUI
    window <- windowNew
    window `on` deleteEvent $ liftIO mainQuit >> return False
    -- i.e., on window deleteEvent (liftIO mainQuit >> return False)
    table <- tableNew 4 2 False
    vls <- forM (zip [0..] ["ACC","AR","DR","PC"]) $ \(n,l) -> do
        lbl <- labelNew (Just l)
        val <- labelNew (Just "")
        tableAttach table lbl 1 2 n (n+1) [] [] 0 0
        tableAttach table val 2 3 n (n+1) [] [] 0 0
        return val
    da <- drawingAreaNew
    set da [widgetWidthRequest := 640, widgetHeightRequest := 200]
    tableAttach table da 0 1 0 4 [] [] 0 0
    set window [containerChild := table]
    widgetShowAll window
    dw <- widgetGetDrawWindow da
    pb <- pixbufNew ColorspaceRgb False 8 640 200
    gc <- gcNew dw
    da `on` exposeEvent $ liftIO (drawPixbuf dw gc pb 0 0 0 0 640 200 RgbDitherNone 0 0) >> return True
    stateRef <- newIORef $ ProcState 0 0 0 0 prog initFB [] []
    intRef <- newIORef $ interpret
    timeoutAdd (fmap last . replicateM 100 $ do
        s <- readIORef stateRef
        i <- readIORef intRef
        let (mi', s') = stepInterpret i s
        writeIORef stateRef s'
        case mi' of
            Just i' -> writeIORef intRef i' >> return True
            Nothing -> return False) 1
    timeoutAdd (do 
        s <- readIORef stateRef
--        print $ Map.size $ stateFB s
        drawFB pb (stateFB s)
        drawWindowInvalidateRect dw (Rectangle 0 0 640 200) True
        labelSetText (vls !! 0) (show (stateACC s))
        labelSetText (vls !! 1) (show (stateAR s))
        labelSetText (vls !! 2) (show (stateDR s))
        labelSetText (vls !! 3) (show (statePC s))
        return True) 100
    mainGUI

