import Graphics.UI.GLUT
import Data.List
import Data.Time
import Data.Complex
import Data.IORef
import Control.Monad
import System.Exit

g t = sin t
gg t = - cos t

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 520 520
    initialDisplayMode $= [ RGBAMode, DoubleBuffered, WithSamplesPerPixel 4 ]
    windowPosition $= Position 100 100
    _window <- createWindow "Fouier"
    time <- getCurrentTime
    tick <- newIORef time
    frame <- newIORef 0.0
    time <- newIORef 0.0
    f <- newIORef $ 1 / pi
    fDir <- newIORef 0
    points <- newIORef ([] :: [(Double, Double)])

    keyboardMouseCallback $= Just (keyboardProc fDir)
    displayCallback $= display points
    idleCallback $= Just (idle _window tick frame time f fDir points)

    windowPosition $= Position 600 0
    _window2 <- createWindow "Wave"
    keyboardMouseCallback $= Just (keyboardProc fDir)
    displayCallback $= displayWave
    postRedisplay $ Just _window2

    mainLoop

displayWave = do
    clear [ColorBuffer]

    renderPrimitive Lines $ do
        vertex $ Vertex2 (0.0) (-1.0 :: Double)
        vertex $ Vertex2 (0.0) (1.0 :: Double)
        vertex $ Vertex2 (-1.0) (0.0 :: Double)
        vertex $ Vertex2 (1.0) (0.0 :: Double)
    renderPrimitive LineStrip $
        mapM_ (\x -> vertex $ Vertex2 (x / 100) (g (x / 5))) . take 201 $ iterate (+1) (-100 :: Double)

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

idle :: RealFloat a =>
          Window
          -> IORef UTCTime
          -> IORef NominalDiffTime
          -> IORef NominalDiffTime
          -> IORef a
          -> IORef a
          -> IORef [(a, a)]
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
        modifyIORef' frameRef ((-) 0.0167)
        fDir <- readIORef fDirRef
        modifyIORef fRef (+ 0.0005 * fDir)
        f <- readIORef fRef
        let t = realToFrac time
        let newPoints = map (\t -> let r :+ i = g (t:+0) * exp (-2 * pi * (0:+1) * (t:+0) * (f:+0)) in (r, i)) . take 2000 $ iterate ((+) 0.0167) t
        writeIORef pointsRef newPoints
        postRedisplay $ Just window

keyboardProc :: Num a =>
                  IORef a -> Key -> KeyState -> p1 -> p2 -> IO ()
keyboardProc fDirRef ch state _ _
    | ch == Char 'q' = exitSuccess
    | ch == Char 'a' && state == Down = writeIORef fDirRef (-1)
    | ch == Char 's' && state == Down = writeIORef fDirRef 0
    | ch == Char 'd' && state == Down = writeIORef fDirRef 1
    | otherwise      = return ()