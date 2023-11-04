import Cmd
import Control.Concurrent (threadDelay)
import Data.Tree
import GameState
import HandlerGame
import HandlerMenu
import Level
import LevelSeq
import Parser
import System.IO

goGame :: GameState -> IO ()
goGame z =
  if fst (rootLabel $ snd $ zipper z) == F
    then do
      let nlv = nextLevelIdent z
      putStrLn "Level completed!"
      putStrLn $ "Moves: " ++ show (movesCount z) ++ ", Bonus: " ++ show (bonusCount z)
      case nlv of
        Just nlvid -> do
          putStrLn $ "Next level code: " ++ nlvid
          handleSelect goGame goMenu z nlvid
        Nothing -> do
          putStrLn "Congratulations! You completed the game!"
    else do
      putStr "> "
      hFlush stdout
      line <- getLine
      case parseInput parseCmd line of
        Nothing -> do
          putStrLn "I'm sorry, I do not understand. (Type `help` to get the list of available controls.)"
          goGame z
        Just Where -> handleWhere goGame goMenu z
        Just LookAround -> handleLook goGame goMenu z
        Just CheckInventory -> handleCheck goGame goMenu z
        Just (UseInventoryWith s) -> handleUse goGame goMenu z s
        Just DropInventory -> handleDrop goGame goMenu z
        Just PickInventory -> handlePick goGame goMenu z
        Just (Goto t) -> handleGoto goGame goMenu z t
        Just Menu -> goMenu z
        Just HelpGame -> handleHelpGame goGame goMenu z

goMenu :: GameState -> IO ()
goMenu z = do
  putStr "> "
  hFlush stdout
  line <- getLine
  case parseInput parseMenuCmd line of
    Nothing -> do
      putStrLn "I'm sorry, I do not understand. (Type `help` to get the list of available options.)"
      goMenu z
    Just (SelectAnotherLevel s) -> handleSelect goGame goMenu z s
    Just Resume -> goGame z
    Just Reset -> handleReset goGame goMenu z
    Just Quit -> handleQuit goGame goMenu z
    Just HelpMenu -> handleHelpMenu goGame goMenu z
    Just Status -> handleStatus goGame goMenu z

-- the top-level interactive loop
repl :: IO ()
repl = do
  putStrLn "Welcome to Treemove.\n"
  putStrLn "You are at the first level."
  handleReset
    goGame
    goMenu
    firstLevel

main = repl
