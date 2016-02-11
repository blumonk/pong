module Main where 

import Graphics.Gloss.Rendering as RS
import Control.Concurrent (threadDelay)
import Control.Monad.State.Strict
import Data
import Util
import Render

main :: IO ()
main = do
    glossState <- initState
    withWindow width height "Super Pong" $ \win -> do
      (_, b) <- runStateT (loop win glossState) initialState
      showWinner b win glossState  
      threadDelay 1000000

