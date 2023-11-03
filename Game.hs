import Cmd
import Control.Concurrent (threadDelay)
import GameState
import HandlerGame
import HandlerMenu
import Level
import Parser
import System.IO

goGame :: GameState -> IO ()
goGame z = do
  putStr "> "
  hFlush stdout
  line <- getLine
  case parseInput parseCmd line of
    Nothing -> do
      putStrLn "I'm sorry, I do not understand."
      goGame z
    Just LookAround -> handleLook goGame goMenu z
    Just CheckInventory -> handleCheck goGame goMenu z
    Just (UseInventoryWith s) -> handleUse goGame goMenu z s
    Just DropInventory -> handleDrop goGame goMenu z
    Just (Goto t) -> handleGoto goGame goMenu z
    Just Menu -> goMenu z

goMenu :: GameState -> IO ()
goMenu z = do
  putStr "> "
  hFlush stdout
  line <- getLine
  case parseInput parseMenuCmd line of
    Nothing -> do
      putStrLn "I'm sorry, I do not understand."
      goMenu z
    Just (SelectAnotherLevel s) -> handleSelect goGame goMenu z s
    Just Resume -> goGame z
    Just Reset -> handleReset goGame goMenu z
    Just Quit -> handleQuit goGame goMenu z

-- the top-level interactive loop
repl :: IO ()
repl = do
  putStrLn "Welcome to Treemove.\n"
  putStrLn "You are at the example level."
  goGame $
    GameState
      { levelIdent = "T1",
        initialLevel = t1,
        zipper = (Hole, t1),
        movesCount = 0,
        bonusCount = 0
      }

main = repl
