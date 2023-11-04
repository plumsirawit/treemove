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

-- changes W to E, Gate to E
mutateNode :: LZip -> Maybe LZip
mutateNode (cxt, tr) =
  case fst $ rootLabel tr of
    W -> Just (cxt, Node (E, snd $ rootLabel tr) (subForest tr))
    Gate _ -> Just (cxt, Node (E, snd $ rootLabel tr) (subForest tr))
    _ -> Nothing

mutateTree :: GoDir -> LZip -> LZip
mutateTree dir z =
  let cur = snd $ rootLabel $ snd z
   in case dir of
        Up k -> fromJust $ goDown (fromJust $ mutateNode $ fromJust $ goUp z) cur
        Down k -> fromJust $ goUp $ fromJust $ mutateNode $ fromJust $ goDown z (snd k)

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
                  zipper = zipper z,
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
      putStrLn $ "Picking " ++ show item ++ " up."
      goGame
        ( GameState
            { levelIdent = levelIdent z,
              initialLevel = initialLevel z,
              zipper = zipper z,
              movesCount = movesCount z,
              bonusCount = bonusCount z,
              inventory = item
            }
        )

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
                            zipper = (ncxt, Node (E, snd (rootLabel ntr)) (subForest ntr)),
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
            else do
              putStrLn "You cannot go there."
              goGame z
        Nothing -> do
          putStrLn "You cannot go there."
          goGame z