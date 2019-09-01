import Graphics.UI.GLUT
import Data.List
import Data.Time
import Data.Complex
import Data.IORef
import Control.Monad
import System.Exit

g = sin

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 520 520
    initialDisplayMode $= [ RGBAMode, DoubleBuffered, WithSamplesPerPixel 4 ]
    windowPosition $= Position 100 100
    _window <- createWindow "Fouier"
    time <- getCurrentTime
    tickRef <- newIORef time
    frameRef <- newIORef 0.0
    timeRef <- newIORef 0.0
    fRef <- newIORef $ 1 / pi
    fDirRef <- newIORef 0
    points <- newIORef ([] :: [(Double, Double)])

    keyboardMouseCallback $= Just (keyboardProc fDirRef)
    displayCallback $= display points
    idleCallback $= Just (idle _window tickRef frameRef timeRef fRef fDirRef points)

    windowPosition $= Position 600 0
    _window2 <- createWindow "Wave"
    keyboardMouseCallback $= Just (keyboardProc fDirRef)
    displayCallback $= displayWave
    postRedisplay $ Just _window2

    mainLoop

displayWave :: IO ()
displayWave = do
    clear [ColorBuffer]

    renderPrimitive Lines $ do
        vertex $ Vertex2 0.0 (-1.0 :: Double)
        vertex $ Vertex2 0.0 (1.0 :: Double)
        vertex $ Vertex2 (-1.0) (0.0 :: Double)
        vertex $ Vertex2 1.0 (0.0 :: Double)
    renderPrimitive LineStrip $
        mapM_ (\x -> vertex $ Vertex2 (x / 50) (g (x / 5) / 10)) . take 201 $ iterate (+1) (-100 :: Double)

    swapBuffers

display :: IORef [(Double, Double)] -> IO ()
display pointsRef = do
    clear [ColorBuffer]
    lineWidth $= 2

    points <- readIORef pointsRef

    renderPrimitive LineStrip $ do
        mapM_ (\(x, y) -> vertex $ Vertex2 y x) points
        vertex $ Vertex2 0.0 (0.0 :: Double)

    let len = genericLength points
    renderPrimitive Points $
        vertex $ Vertex2 ((/len) . sum . map snd $ points) ((/len) . sum . map fst $ points)

    swapBuffers

idle :: Window
          -> IORef UTCTime
          -> IORef NominalDiffTime
          -> IORef NominalDiffTime
          -> IORef Double
          -> IORef Double
          -> IORef [(Double, Double)]
          -> IO ()
idle window tickRef frameRef timeRef fRef fDirRef pointsRef = do
    tick <- readIORef tickRef
    curr <- getCurrentTime

    let diff = diffUTCTime curr tick

    writeIORef tickRef curr
    modifyIORef frameRef (+diff)
    modifyIORef timeRef (+diff)

    time <- readIORef timeRef
    frame <- readIORef frameRef

    when (frame > 0.0167) $ do
        modifyIORef' frameRef (\x -> x - 0.0167)
        fDir <- readIORef fDirRef
        modifyIORef fRef (+ 0.0005 * fDir)
        f <- readIORef fRef
        let realTime = realToFrac time
        let newPoints = map (\t -> let r :+ i = (g t:+0) * exp (-2 * pi * (0:+1) * (t:+0) * (f:+0)) in (r, i)) . take 2000 $ iterate (+0.0167) realTime
        writeIORef pointsRef newPoints
        postRedisplay $ Just window

keyboardProc :: IORef Double -> Key -> KeyState -> p1 -> p2 -> IO ()
keyboardProc fDirRef ch state _ _
    | ch == Char 'q' = exitSuccess
    | ch == Char 'a' && state == Down = writeIORef fDirRef (-1)
    | ch == Char 's' && state == Down = writeIORef fDirRef 0
    | ch == Char 'd' && state == Down = writeIORef fDirRef 1
    | otherwise      = return ()