module Logic where

import Data

-- Checks the ball's position. If somebody scored, returns the ball to the initial state 
-- and updates the score.
checkPoint :: GameState -> GameState
checkPoint gs 
    | x < -(fromIntegral width) / 2 = gs { score = (p1, p2 + 1), ballVel = (0, 0), ballPos = (0, 0) }
    | x >  (fromIntegral width) / 2 = gs { score = (p1 + 1, p2), ballVel = (0, 0), ballPos = (0, 0) }
    | otherwise = gs
  where
    (x , _ ) = ballPos gs
    (p1, p2) = score gs

-- Updates the ball's position and velocity. 
-- If wall or paddle collision occurs, updates the ball's velocity vector.
-- Otherwise, moves the ball along the velocity vector.
moveBall :: GameState -> GameState
moveBall gs = gs { ballVel = (vX', vY'), ballPos = (x + vX', y + vY') }
  where
    (x , y ) = ballPos gs
    (vX, vY) = ballVel gs
    (y1, y2) = (player1 gs, player2 gs)

    wallCollision = (y + vY) < (-(fromIntegral height) / 2 + ballRadius) ||
                    (y + vY) > ( (fromIntegral height) / 2 - ballRadius)

    lPaddleCollision = (x + vX) < (-(fromIntegral width) / 2 + ballRadius + paddleWidth / 2) &&
                       abs(y - y1) < paddleHeight / 2
    rPaddleCollision = (x + vX) > ( (fromIntegral width) / 2 - ballRadius - paddleWidth / 2) &&
                       abs(y - y2) < paddleHeight / 2
    
    vel :: Float -> Float -> Float -> (Float, Float)
    vel s yB yP = let t = (yB - yP) / 10 in (s * (sqrt $ ballV ** 2 - t ** 2), t)
    (vX', vY') = if wallCollision    then (vX, -vY) else 
                 if lPaddleCollision then vel 1 y y1 else
                 if rPaddleCollision then vel (-1) y y2 else (vX, vY) 

-- Moves the paddles if the corresponding keys were pressed. 
-- Stops the paddles when they hit the walls.
-- (Player1 Up, Player1 Down) -> (Player2 Up, Player2 Down) -> ...
movePaddles :: (Bool, Bool) -> (Bool, Bool) -> GameState -> Float -> GameState
movePaddles keys1 keys2 gs vel = gs { player1 = y1', player2 = y2' }
  where
    bump y = y < (-(fromIntegral height) / 2 + paddleHeight / 2) ||
             y > ( (fromIntegral height) / 2 - paddleHeight / 2)

    move :: (Bool, Bool) -> Float -> Float -> Float
    move (True, _)      pos v    = pos + v
    move (_, True)      pos v    = pos - v
    move (False, False) pos _    = pos

    (y1, y2) = (player1 gs, player2 gs)
    y1' = if bump (move keys1 y1 vel) then y1 else move keys1 y1 vel
    y2' = if bump (move keys2 y2 vel) then y2 else move keys2 y2 vel

