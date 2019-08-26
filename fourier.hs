import Graphics.UI.GLUT
import Data.List
import System.Exit
import Data.IORef
import Data.Time
import Data.Complex

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
    points <- newIORef ([] :: [(Double, Double)])

    keyboardMouseCallback $= Just keyboardProc
    displayCallback $= display points
    idleCallback $= Just (idle display tick frame time points)

    mainLoop
 
display pointsRef = do 
    clear [ColorBuffer]
    lineWidth $= 2
    
    points <- readIORef pointsRef

    renderPrimitive LineStrip $ do
        vertex $ Vertex2 0.0 (0.0 :: Double)
        mapM_ (\(x, y) -> vertex $ Vertex2 y x) points
    
    swapBuffers

idle display tickRef frameRef timeRef pointsRef = do
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
        let fracTime = realToFrac time
        points <- readIORef pointsRef
        let g t = (cos t + 1) / 2
        let f = 1
        let r :+ i = (g fracTime) * exp (-2 * pi * (0 :+ 1) * fracTime * f)
        writeIORef pointsRef $ take 1000 $ (r, i) : points
        display pointsRef
    else
        return ()
    

keyboardProc ch state _ _
    | ch == Char 'q' = exitWith ExitSuccess
    | otherwise      = return ()