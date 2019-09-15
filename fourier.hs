import Graphics.UI.GLUT
import Data.List
import Data.Time
import Data.Complex
import Data.IORef
import Control.Monad
import System.Exit

g t = (sin t + sin (2*t)) / 2

idxLen :: Int
idxLen = 2000

wavePoints :: [(Double, Double)]
wavePoints = map (\x -> (x, g x)) . take idxLen $ iterate (+0.0167) 0

fftPoints :: [(Double, Double)]
fftPoints = map (\f -> let (x:+y) = (/fromIntegral idxLen) . sum $ map (\t -> (g t:+0) * exp (-2 * pi * (0:+1) * (t:+0) * (1/f:+0))) . take idxLen $ iterate (+0.0167) 0 in (f,sqrt $ x**2 + y**2)) . take idxLen $ iterate (+0.0167) 0.0167

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
    let f = 1
    fRef <- newIORef $ f
    fDirRef <- newIORef 0
    idxRef <- newIORef 0
    points <- newIORef $ map (\t -> let r :+ i = (g t:+0) * exp (-2 * pi * (0:+1) * (t:+0) * (1/f:+0)) in (r, i)) . take idxLen $ iterate (+0.0167) 0

    keyboardMouseCallback $= Just (keyboardProc fDirRef)
    displayCallback $= display fRef points idxRef

    windowPosition $= Position 600 0
    waveWindow <- createWindow "Wave"
    displayCallback $= displayWave idxRef

    windowPosition $= Position 0 600
    fftWindow <- createWindow "fft"
    displayCallback $= displayFft fRef

    keyboardMouseCallback $= Just (keyboardProc fDirRef)
    idleCallback $= Just (idle _window waveWindow fftWindow tickRef frameRef fRef fDirRef points idxRef)

    mainLoop

displayFft :: IORef Double -> IO ()
displayFft fRef = do
    clear [ColorBuffer]

    f <- readIORef fRef

    renderPrimitive Lines $ do
        vertex $ Vertex2 (-0.9) (-1.0 :: Double)
        vertex $ Vertex2 (-0.9) (1.0 :: Double)
        vertex $ Vertex2 (-1.0) (0.0 :: Double)
        vertex $ Vertex2 1.0 (0.0 :: Double)

    renderPrimitive LineStrip $
        mapM_ (\(x,y) -> vertex $ Vertex2 (x / 4 - 0.9) (y)) fftPoints

    let fOrd a b = abs (f - fst a) `compare` abs (f - fst b)
    let (nX, nY) = minimumBy fOrd fftPoints

    renderPrimitive Lines $ do
        vertex $ Vertex2 (nX / 4 - 0.9) (nY)
        vertex $ Vertex2 (nX / 4 - 0.9) 0

    swapBuffers

displayWave :: IORef Int -> IO ()
displayWave idxRef = do
    clear [ColorBuffer]

    idx <- readIORef idxRef

    renderPrimitive Lines $ do
        vertex $ Vertex2 (-0.9) (-1.0 :: Double)
        vertex $ Vertex2 (-0.9) (1.0 :: Double)
        vertex $ Vertex2 (-1.0) (0.0 :: Double)
        vertex $ Vertex2 1.0 (0.0 :: Double)

    renderPrimitive LineStrip $
        mapM_ (\(x, y) -> vertex $ Vertex2 (x / 5 - 0.9) (y / 5)) wavePoints

    renderPrimitive Lines $ do
        vertex $ Vertex2 (fromIntegral idx * 0.0167 / 5 - 0.9) (0.0 :: Double)
        vertex $ Vertex2 (fromIntegral idx * 0.0167 / 5 - 0.9) (snd (wavePoints !! idx) / 5)

    swapBuffers

display :: IORef Double -> IORef [(Double, Double)] -> IORef Int -> IO ()
display fRef pointsRef idxRef = do
    clear [ColorBuffer]
    lineWidth $= 2
    pointSize $= 3

    f <- readIORef fRef
    points <- readIORef pointsRef
    idx <- readIORef idxRef

    color $ Color3 1 1 (1 :: Double)
    renderPrimitive Lines $ do
        let (x, y) = points !! idx
        vertex $ Vertex2 0.0 (0.0 :: Double)
        vertex $ Vertex2 x y

    renderPrimitive LineStrip $
        mapM_ (\(x, y) -> vertex $ Vertex2 x y) points

    rasterPos (Vertex2 0.4 (-0.95) :: Vertex2 Float)
    renderString Fixed8By13 $ "Interval : " ++ take 8 (show f)

    swapBuffers

idle :: Window
          -> Window
          -> Window
          -> IORef UTCTime
          -> IORef NominalDiffTime
          -> IORef Double
          -> IORef Double
          -> IORef [(Double, Double)]
          -> IORef Int
          -> IO ()
idle window waveWindow fftWindow tickRef frameRef fRef fDirRef pointsRef idxRef = do
    tick <- readIORef tickRef
    curr <- getCurrentTime

    let diff = diffUTCTime curr tick

    writeIORef tickRef curr
    modifyIORef frameRef (+diff)

    frame <- readIORef frameRef

    when (frame > 0.0167) $ do
        modifyIORef' frameRef (\x -> x - 0.0167)
        fDir <- readIORef fDirRef
        modifyIORef fRef (+ 0.005 * fDir)
        f <- readIORef fRef
        idx <- readIORef idxRef
        if idx >= idxLen - 1 then
            modifyIORef idxRef (\x -> x - idx + 1)
        else
            modifyIORef idxRef (+1)

        when (fDir /= 0) $ do
            let newPoints = map (\t -> let r :+ i = (g t:+0) * exp (-2 * pi * (0:+1) * (t:+0) * (1/f:+0)) in (r, i)) . take idxLen $ iterate (+0.0167) 0
            writeIORef pointsRef newPoints
        postRedisplay $ Just window
        postRedisplay $ Just waveWindow
        postRedisplay $ Just fftWindow

keyboardProc :: IORef Double -> Key -> KeyState -> p1 -> p2 -> IO ()
keyboardProc fDirRef ch state _ _
    | ch == Char 'q' = exitSuccess
    | ch == Char 'a' && state == Down = writeIORef fDirRef (-1)
    | ch == Char 's' && state == Down = writeIORef fDirRef 0
    | ch == Char 'd' && state == Down = writeIORef fDirRef 1
    | otherwise      = return ()