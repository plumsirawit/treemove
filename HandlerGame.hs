module HandlerGame where

import Data.List
import Data.Maybe (fromJust)
import Data.Tree
import GameState
import Level

data GoDir = Up LNode | Down LNode

getDir :: LZip -> String -> Maybe GoDir
getDir (cxt, tr) t =
  let chs = map rootLabel $ subForest tr
   in case find (\x -> snd x == t) chs of
        Just ch -> Just (Down ch)
        Nothing ->
          if cxt == Hole
            then Nothing
            else
              let (TreeHole k _ _) = cxt
               in if snd k == t
                    then Just (Up k)
                    else Nothing

nodeType :: GoDir -> LType
nodeType dir = fst $ case dir of
  Up x -> x
  Down x -> x

handleWhere :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleWhere goGame goMenu z = do
  let (cxt, tr) = zipper z
  let k = rootLabel tr
  putStrLn $ "You're at " ++ show (fst k) ++ "(" ++ snd k ++ ")."
  goGame z

handleLook :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleLook goGame goMenu z = do
  let (cxt, tr) = zipper z
  let chs = map rootLabel $ subForest tr
  let nbhood = case cxt of
        Hole -> chs
        TreeHole k _ _ -> k : chs
  putStrLn $ intercalate ", " $ map (\x -> show (fst x) ++ "(" ++ snd x ++ ")") nbhood
  goGame z

handleCheck :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleCheck goGame goMenu z = do
  case inventory z of
    Just item -> putStrLn $ "You have " ++ show item ++ " in your inventory."
    Nothing -> putStrLn "You have nothing in your inventory."
  goGame z

mutateNode :: LZip -> LType -> LZip
mutateNode (cxt, tr) lt = (cxt, Node (lt, snd $ rootLabel tr) (subForest tr))

mutateTree :: GoDir -> LZip -> LZip
mutateTree dir z =
  let cur = snd $ rootLabel $ snd z
   in case dir of
        Up k -> fromJust $ goDown (mutateNode (fromJust $ goUp z) E) cur
        Down k -> fromJust $ goUp $ mutateNode (fromJust $ goDown z (snd k)) E

handleUse :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> String -> IO ()
handleUse goGame goMenu z t = do
  case getDir (zipper z) t of
    Nothing -> do
      putStrLn $ "Node " ++ t ++ " not found."
      goGame z
    Just dir -> do
      case inventory z of
        Nothing -> do
          putStrLn "Inventory is empty."
          goGame z
        Just item -> do
          case (nodeType dir, item) of
            (W, ItemBox) ->
              goGame
                GameState
                  { levelIdent = levelIdent z,
                    initialLevel = initialLevel z,
                    zipper = mutateTree dir (zipper z),
                    movesCount = movesCount z,
                    bonusCount = bonusCount z,
                    inventory = Nothing
                  }
            (Gate x, ItemKey y) ->
              if x == y
                then
                  goGame
                    GameState
                      { levelIdent = levelIdent z,
                        initialLevel = initialLevel z,
                        zipper = mutateTree dir (zipper z),
                        movesCount = movesCount z,
                        bonusCount = bonusCount z,
                        inventory = Nothing
                      }
                else do
                  putStrLn "Invalid key."
                  goGame z
            _ -> do
              putStrLn $ "Cannot use item " ++ show item ++ " with node " ++ show (nodeType dir)
              goGame z

handleDrop :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleDrop goGame goMenu z =
  if fst (rootLabel $ snd $ zipper z) /= E
    then do
      putStrLn "The current node is not empty."
      goGame z
    else do
      let item = inventory z
      case item of
        Just it -> do
          putStrLn $ "Dropping " ++ show it ++ " from your inventory."
          goGame
            ( GameState
                { levelIdent = levelIdent z,
                  initialLevel = initialLevel z,
                  zipper = mutateNode (zipper z) (toType it),
                  movesCount = movesCount z,
                  bonusCount = bonusCount z,
                  inventory = Nothing
                }
            )
        Nothing -> do
          putStrLn "Nothing to drop."
          goGame z

handlePick :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handlePick goGame goMenu z = do
  let item = getItem (zipper z)
  case inventory z of
    Just item -> do
      putStrLn "Your inventory is full."
      goGame z
    Nothing -> do
      case item of
        Just it -> do
          putStrLn $ "Picking " ++ show it ++ " up."
          goGame
            ( GameState
                { levelIdent = levelIdent z,
                  initialLevel = initialLevel z,
                  zipper = mutateNode (zipper z) E,
                  movesCount = movesCount z,
                  bonusCount = bonusCount z,
                  inventory = item
                }
            )
        Nothing -> do
          putStrLn "Nothing to pick."
          goGame z

handleGoto :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> String -> IO ()
handleGoto goGame goMenu z t = do
  let (cxt, tr) = zipper z
  case getDir (cxt, tr) t of
    Nothing -> do
      putStrLn $ "Node " ++ t ++ " not found."
      goGame z
    Just dir -> case dir of
      Up _ -> case goUp (cxt, tr) of
        Just (ncxt, ntr) ->
          if checkLegal (ncxt, ntr)
            then
              ( if fst (rootLabel ntr) == Z
                  then
                    goGame
                      ( GameState
                          { levelIdent = levelIdent z,
                            initialLevel = initialLevel z,
                            zipper = mutateNode (ncxt, ntr) E,
                            movesCount = 1 + movesCount z,
                            bonusCount = 1 + bonusCount z,
                            inventory = inventory z
                          }
                      )
                  else
                    goGame
                      ( GameState
                          { levelIdent = levelIdent z,
                            initialLevel = initialLevel z,
                            zipper = (ncxt, ntr),
                            movesCount = 1 + movesCount z,
                            bonusCount = bonusCount z,
                            inventory = inventory z
                          }
                      )
              )
            else do
              putStrLn "You cannot go there."
              goGame z
        Nothing -> do
          putStrLn "You cannot go there."
          goGame z
      Down tt -> case goDown (cxt, tr) (snd tt) of
        Just (ncxt, ntr) ->
          if checkLegal (ncxt, ntr)
            then
              ( if fst (rootLabel ntr) == Z
                  then
                    goGame
                      ( GameState
                          { levelIdent = levelIdent z,
                            initialLevel = initialLevel z,
                            zipper = mutateNode (ncxt, ntr) E,
                            movesCount = 1 + movesCount z,
                            bonusCount = 1 + bonusCount z,
                            inventory = inventory z
                          }
                      )
                  else
                    goGame
                      ( GameState
                          { levelIdent = levelIdent z,
                            initialLevel = initialLevel z,
                            zipper = (ncxt, ntr),
                            movesCount = 1 + movesCount z,
                            bonusCount = bonusCount z,
                            inventory = inventory z
                          }
                      )
              )
            else do
              putStrLn "You cannot go there."
              goGame z
        Nothing -> do
          putStrLn "You cannot go there."
          goGame z

handleHelpGame :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleHelpGame goGame goMenu z = do
  putStrLn "====================== List of available controls ======================"
  putStrLn "`where`                  -- ask for your current position"
  putStrLn "`look around`            -- see types and names of the neighboring nodes"
  putStrLn "`check inventory`        -- check your current inventory"
  putStrLn "`use inventory with <L>` -- use your current inventory with <L>"
  putStrLn "                            (note that <L> has to be a neighboring node)"
  putStrLn "`drop inventory`         -- drop your current item from your inventory"
  putStrLn "                            (note that the current node has to be empty)"
  putStrLn "`pick inventory`         -- pick up the item in the current node"
  putStrLn "                            (note that your inventory must be empty)"
  putStrLn "`go to <L>`              -- move to <L>"
  putStrLn "                            (note that <L> has to be a neighboring node)"
  putStrLn "`menu`                   -- show the game menu"
  putStrLn "`help`                   -- show this list of available controls"
  putStrLn "========================================================================"
  goGame z