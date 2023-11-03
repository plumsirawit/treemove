module HandlerGame where

import GameState
import Level

handleLook :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleLook goGame goMenu z = undefined -- todo

handleCheck :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleCheck = undefined -- todo

handleUse :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> String -> IO ()
handleUse = undefined -- todo

handleDrop :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleDrop = undefined -- todo

handleGoto :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleGoto = undefined -- todo