{-# LANGUAGE BangPatterns #-}
module PlGraphics (
    display,
    particolour
) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import System.Exit (exitSuccess)
import PlDefinitions
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
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    GL.ortho 0 (realToFrac w) (realToFrac h) 0 (-1000) 1000

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
    GLFW.windowHint (GLFW.WindowHint'DepthBits $ Just 24)
    GLFW.windowHint (GLFW.WindowHint'DoubleBuffer True)

    GLFW.windowHint (GLFW.WindowHint'Resizable True)
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


    stdGen <- initStdGen

    let ncolours = 7
    -- Particle count in the simulation
    let pcount = 5000

    let (mtx, stdGen2) = generateRandomForceMatrix ncolours stdGen
    let sp = PLifeSP {
      colours = ncolours,
      width = 1280,
      height = 720,
      depth = 500,
      wforcemult = 1.0,
      pforcemult = 10.0,
      forceMatrix = mtx
      }

    let (istate, _) = generateInitialState sp pcount stdGen2

    inWindow <- openWindow "Particle Life" (width sp, height sp)
    resizeWindow inWindow (width sp) (height sp)


    putStrLn $ "Using parameters: " ++ (show sp)

    currentTime <- GLFW.getTime
    case currentTime of 
       Nothing -> return ()
       Just t -> do
          onDisplay inWindow sp istate t
          closeWindow inWindow

-- Render loop, also calls the logic    
onDisplay :: GLFW.Window -> SimulationParameters -> ParticleState -> Double -> IO ()
onDisplay win sp state ptime =
  do
    currentTime <- GLFW.getTime
    case currentTime of
        Nothing -> return ()
        Just t -> do
          (w, h) <- GLFW.getFramebufferSize win
          let !sp' = sp {width = w, height = h}

          GL.clearColor $= Color4 0.2 0.2 0.2 1
          GL.depthFunc $= Just GL.Less
          GL.clear [ColorBuffer, DepthBuffer]

          let !newstate = simulateStep sp' (t - ptime) state
          renderState newstate

          GL.flush

          GLFW.swapBuffers win
        
          GLFW.pollEvents
          onDisplay win sp' newstate t

renderState :: ParticleState -> IO ()
renderState state = do 
  GL.pointSize $= 3
  GL.renderPrimitive GL.Points $ mapM_ renderParticle state

renderParticle :: Particle -> IO ()
renderParticle p = 
  do
    let (x, y, z) = PlDefinitions.position p
    GL.currentColor $= (multiCol ((z + 200.0)/700.0) $ particolour (colourIdx p))
    GL.vertex $ particlePosition p

multiCol :: Double -> Color4 GL.GLfloat -> Color4 GL.GLfloat
multiCol fv (Color4 r g b a) = Color4 nr ng nb a
                              where
                                v = realToFrac fv
                                (nr, ng, nb) = (r * v, g * v, b * v)

particlePosition :: Particle -> GL.Vertex3 GL.GLfloat
particlePosition p = GL.Vertex3 x y z
                    where 
                      (xd, yd, zd) = ParticleLife.position p
                      x = realToFrac xd
                      y = realToFrac yd
                      z = realToFrac zd


-- Gets the rendering colour of the particle
particolour :: Int -> GL.Color4 GL.GLfloat
particolour 1 = Color4 0.6 0.6 1.0 1
particolour 2 = Color4 1.0 0.6 0.6 1
particolour 3 = Color4 0.6 1.0 0.6 1
particolour 4 = Color4 1.0 1.0 0.6 1
particolour 5 = Color4 0.6 1.0 1.0 1
particolour 6 = Color4 1.0 0.6 1.0 1
particolour 7 = Color4 1.0 0.8 0.9 1
particolour x = Color4 r g b 1 
              where
                (r, g1) = uniformR (0.0, 1.0) (mkStdGen (42 + x))
                (g, g2) = uniformR (0.0, 1.0) g1
                (b, _) = uniformR (0.0, 1.0) g2
                  