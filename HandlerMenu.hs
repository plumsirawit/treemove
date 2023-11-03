module HandlerMenu where

import GameState
import Level

handleSelect :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> String -> IO ()
handleSelect goGame goMenu z s =
  let newlv = loadLevel s
   in goGame $
        GameState
          { levelIdent = s,
            initialLevel = newlv,
            zipper = (Hole, newlv),
            movesCount = 0,
            bonusCount = 0
          }

handleReset :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleReset goGame goMenu z =
  goGame $
    GameState
      { levelIdent = levelIdent z,
        initialLevel = initialLevel z,
        zipper = (Hole, initialLevel z),
        movesCount = 0,
        bonusCount = 0
      }

handleQuit :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleQuit goGame goMenu z = do
  putStrLn "Goodbye."
  return ()