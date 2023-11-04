module HandlerMenu where

import Data.Maybe (fromJust)
import GameState
import Level
import LevelSeq

handleSelect :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> String -> IO ()
handleSelect goGame goMenu z s = do
  let newlv = loadLevel s
  case newlv of
    Just nlvid ->
      goGame $
        GameState
          { levelIdent = s,
            initialLevel = nlvid,
            zipper = fromJust $ zipToStart nlvid,
            movesCount = 0,
            bonusCount = 0,
            inventory = inventory z
          }
    Nothing -> do
      putStrLn $ "Failed to load level " ++ s ++ "!"
      goGame z

handleReset :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleReset goGame goMenu z =
  goGame $
    GameState
      { levelIdent = levelIdent z,
        initialLevel = initialLevel z,
        zipper = fromJust $ zipToStart $ initialLevel z,
        movesCount = 0,
        bonusCount = 0,
        inventory = inventory z
      }

handleQuit :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleQuit goGame goMenu z = do
  putStrLn "Goodbye."
  return ()