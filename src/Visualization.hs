{-# LANGUAGE OverloadedStrings #-}

module Visualization where

import qualified Compiler as C
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import Data.IORef
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import qualified Simulation as S
import System.Exit (exitFailure)
import System.IO

visualize :: [S.Micro16State] -> IO ()
visualize states = do
  GLFW.setErrorCallback (Just errorCallback)
  True <- GLFW.init
  Just window <- GLFW.createWindow 800 600 "Micro16 Simulator" Nothing Nothing
  GLFW.makeContextCurrent (Just window)
  shader1 <- loadShader1
  GL.currentProgram $= Just shader1
  b <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just b
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
  currentStateIndex <- newIORef 0
  whileM_ (not <$> GLFW.windowShouldClose window) $ do
    GL.clear [GL.ColorBuffer]
    index <- readIORef currentStateIndex
    writeIORef currentStateIndex ((index + 1) `mod` length states)
    renderState $ states !! index
    GLFW.swapBuffers window
    threadDelay (1000 * 1000)
    GLFW.pollEvents
  GLFW.destroyWindow window
  GLFW.terminate

renderState :: S.Micro16State -> IO ()
renderState state = return ()

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ p f = go
  where
    go = do
      x <- p
      when x $ f >> go

errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

loadShader1 :: IO GL.Program
loadShader1 = do
  text <- B.readFile "assets/shaders/shader1.glsl"
  vert <- makeShader GL.VertexShader text
  frag <- makeShader GL.FragmentShader text
  program <- GL.createProgram
  mapM_ (GL.attachShader program) [vert, frag]
  GL.linkProgram program
  linked <- GL.get (GL.linkStatus program)
  GL.validateProgram program
  valid <- GL.get (GL.validateStatus program)
  unless (linked && valid) $ GL.get (GL.programInfoLog program) >>= logAndExit
  return program

makeShader :: GL.ShaderType -> B.ByteString -> IO GL.Shader
makeShader shaderType text = do
  s <- GL.createShader shaderType
  GL.shaderSourceBS s $=
    B.concat
      ["#version 330 core\n#define ", GL.packUtf8 (show shaderType), "\n", text]
  GL.compileShader s
  success <- GL.get (GL.compileStatus s)
  unless success $ GL.get (GL.shaderInfoLog s) >>= logAndExit
  return s

logAndExit :: String -> IO ()
logAndExit s = hPutStrLn stderr s >> exitFailure

