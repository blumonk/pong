{-# LANGUAGE PackageImports #-}

module Render where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering as RS
import Control.Monad.State.Strict
import Control.Concurrent (threadDelay)
import Data
import Logic
import Util
 
-- The main loop.
-- 1) Delay the picture
-- 2) Poll events from the window
-- 3) Update game state
-- 4) Finish if the game ended or somebody pressed 'esc'
loop :: Window -> RS.State -> StateT GameState IO ()
loop window glossState = do
    lift $ threadDelay 15000
    lift $ pollEvents
    q <- lift $ keyIsPressed window Key'Escape
    w <- lift $ keyIsPressed window Key'W
    s <- lift $ keyIsPressed window Key'S
    u <- lift $ keyIsPressed window Key'Up
    d <- lift $ keyIsPressed window Key'Down
    r <- lift $ keyIsPressed window Key'Space 
    currState <- get
    let newState = if paused currState && r 
                   then currState { ballVel = initialVel }
                   else checkPoint $ moveBall $ movePaddles (w,s) (u,d) currState 10
    let (pts1, pts2) = score newState
    put newState
    lift $ renderState newState window glossState
    unless (q || pts1 >= maxScore || pts2 >= maxScore) $ loop window glossState

-- Renders the result of the game.
showWinner :: GameState -> GLFW.Window -> RS.State -> IO ()
showWinner gs win glossState = do 
    displayPicture (width, height) (light blue) glossState 1.0 $ txt 
    swapBuffers win
  where
    (p1, p2) = score gs
    winner = if p1 == p2 then "Draw!" else if p1 > p2 then "Player 1 wins!" else "Player 2 wins!"
    temp = color yellow $ scale 0.3 0.3 $ text $ winner
    txt = pictures [ translate (-150) 0 $ temp, translate (-149) 1 $ temp, translate (-150) 1 $ temp]

paused :: GameState -> Bool
paused gs = (ballVel gs) == (0, 0) && (ballPos gs) == (0, 0)

-- Renders the current game state within the window
renderState :: GameState -> GLFW.Window -> RS.State -> IO()
renderState gs win glossState = do
    displayPicture (width, height) white glossState 1.0 $ pictures [paddles, ball, points]
    swapBuffers win
  where
    (ypos1, ypos2)  = (player1 gs, player2 gs)
    (pts1, pts2)    = score gs
    (ballX, ballY)  = ballPos gs
    paddlePic x y c = translate x y $ color c $ rectangleSolid paddleWidth paddleHeight
    scorePic x s    = translate x scoreY $ color black $ scale 0.2 0.2 $ text (show s)

    paddles = pictures [paddlePic (-paddleShift) ypos1 black,
                        paddlePic  (paddleShift) ypos2 black]
    points  = pictures [scorePic (-scoreShift) pts1,
                        scorePic  (scoreShift) pts2]
    ball    = translate ballX ballY $ color red $ circleSolid ballRadius

