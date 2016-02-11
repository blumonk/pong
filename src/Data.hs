module Data where

import Graphics.Gloss (Point)

-- The size of the main window
width, height :: Int
(width, height) = (640, 480)

-- The size of the paddles
paddleWidth, paddleHeight :: Float
(paddleWidth, paddleHeight) = (10, 60)

-- The score on which the game ends
maxScore :: Int
maxScore = 5

-- The offset of the score 
scoreShift, scoreY :: Float
(scoreShift, scoreY) = (50, 200)

paddleShift, ballRadius, ballV :: Float
paddleShift = 315
ballRadius = 10
ballV = 6

-- The initial velocity of the ball
initialVel :: (Float, Float)
initialVel = (sqrt (ballV ** 2 / 2), sqrt (ballV ** 2 / 2))

initialState :: GameState
initialState = GS {
    player1 = -10,
    player2 = 10,
    ballPos = (0, 0),
    ballVel = (0, 0),
    score   = (0, 0)
}

data GameState = GS {
    player1 :: Float,
    player2 :: Float,
    ballPos :: Point,
    ballVel :: (Float, Float),
    score   :: (Int, Int)
} deriving Show

