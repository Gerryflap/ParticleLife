module PlGraphics (
    display
) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad (forever)
import System.Exit (exitSuccess)
import ParticleLife
import System.Random

keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()
                                                                  
shutdown :: GLFW.WindowCloseCallback
shutdown win =
  do
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitSuccess
    return ()                                                                  
     
resizeWindow :: GLFW.WindowSizeCallback
resizeWindow _ w h =
  do
    GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0   

openWindow :: String -> (Int, Int) -> IO GLFW.Window
openWindow title (sizex,sizey) =
  do
    _ <- GLFW.init
    GLFW.defaultWindowHints
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 5)
    -- Use core if you want compatibility with more modern stacks
    --GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Compat)

    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    Just win <- GLFW.createWindow sizex sizey title Nothing Nothing
    GLFW.makeContextCurrent (Just win)
    GLFW.setWindowSizeCallback win (Just resizeWindow)
    GLFW.setKeyCallback win (Just keyPressed)
    GLFW.setWindowCloseCallback win (Just shutdown)
    return win

closeWindow :: GLFW.Window -> IO ()
closeWindow win =
  do
    GLFW.destroyWindow win
    GLFW.terminate

display :: IO ()
display =
  do
    inWindow <- openWindow "Particle Life" (512,512)
    resizeWindow inWindow 512 512

    stdGen <- initStdGen
    let (istate, _) = generateInitialState 1000 stdGen

    currentTime <- GLFW.getTime
    case currentTime of 
       Nothing -> return ()
       Just t -> do
          onDisplay inWindow istate t
          closeWindow inWindow

-- Render loop, also calls the logic    
onDisplay :: GLFW.Window -> ParticleState -> Double -> IO ()
onDisplay win state ptime =
  do
    currentTime <- GLFW.getTime
    case currentTime of
        Nothing -> return ()
        Just t -> do
    
          GL.clearColor $= Color4 0.2 0.2 0.2 1
          GL.clear [ColorBuffer]

          let newstate = simulateStep (t - ptime) state
          renderState newstate

          GL.flush

          GLFW.swapBuffers win
        
          forever $ do
            GLFW.pollEvents
            onDisplay win newstate t

renderState :: ParticleState -> IO ()
renderState state = do 
  GL.pointSize $= 3
  GL.renderPrimitive GL.Points $ mapM_ renderParticle state

renderParticle :: Particle -> IO ()
renderParticle p = 
  do
    GL.currentColor $= particolour (colourIdx p)
    GL.vertex $ particlePosition p


particlePosition :: Particle -> GL.Vertex3 GL.GLfloat
particlePosition p = GL.Vertex3 x y (0.0 :: GLfloat)
                    where 
                      (xd, yd) = ParticleLife.position p
                      x = realToFrac xd
                      y = realToFrac yd


-- Gets the rendering colour of the particle
particolour :: Int -> GL.Color4 GL.GLfloat
particolour 0 = Color4 0.6 0.6 1 1
particolour 1 = Color4 1 0.6 0.6 1
particolour 2 = Color4 0.6 1 0.6 1
particolour x = particolour (mod x 3)
                  