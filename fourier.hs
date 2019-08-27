import Graphics.UI.GLUT
import Data.List
import System.Exit
import Data.IORef
import Data.Time
import Data.Complex


g t = ((sin t + 1) / 2 + (sin (2 * t) + 1) / 2) / 2

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 520 520
    initialDisplayMode $= [ RGBAMode, DoubleBuffered, WithSamplesPerPixel 4 ]
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
    idleCallback $= Just (idle display tick frame time f fDir points)

    mainLoop

display :: IORef [(Double, Double)] -> IO ()
display pointsRef = do 
    clear [ColorBuffer]
    lineWidth $= 2
    
    points <- readIORef pointsRef

    renderPrimitive LineStrip $ do
        mapM_ (\(x, y) -> vertex $ Vertex2 y x) points
        vertex $ Vertex2 0.0 (0.0 :: Double)
    
    swapBuffers

idle display tickRef frameRef timeRef fRef fDirRef pointsRef = do
    tick <- readIORef tickRef
    curr <- getCurrentTime
    
    let diff = diffUTCTime curr tick

    writeIORef tickRef (curr)
    modifyIORef frameRef (+diff)
    modifyIORef timeRef (+diff)
    
    time <- readIORef timeRef
    frame <- readIORef frameRef

    if frame > 0.0167 then do
        modifyIORef' frameRef ((-) 0.0167)
        fDir <- readIORef fDirRef
        modifyIORef fRef (+ 0.0005 * fDir)
        f <- readIORef fRef
        let t = realToFrac time
        let newPoints = map (\t -> let r :+ i = (g t) * exp (-2 * pi * (0 :+ 1) * t * f) in (r, i)) . take 2000 $ iterate ((+) 0.0167) t
        writeIORef pointsRef newPoints
        display pointsRef
    else
        return ()

keyboardProc fDirRef ch state _ _
    | ch == Char 'q' = exitWith ExitSuccess
    | ch == Char 'a' && state == Down = do 
        writeIORef fDirRef (-1)
    | ch == Char 's' && state == Down = do 
        writeIORef fDirRef 0
    | ch == Char 'd' && state == Down = do 
        writeIORef fDirRef 1
    | otherwise      = return ()