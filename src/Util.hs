{-# LANGUAGE PackageImports #-}

module Util where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Monad.State.Strict

-- Draw stuff within the window
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow winW winH title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow winW winH title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPressed `fmap` GLFW.getKey win key

isPressed :: KeyState -> Bool
isPressed KeyState'Pressed   = True
isPressed KeyState'Repeating = True
isPressed _                  = False
