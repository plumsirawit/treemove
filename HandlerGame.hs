module HandlerGame where

import Data.List
import Data.Tree
import GameState
import Level

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

handleUse :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> String -> IO ()
handleUse = undefined -- todo

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

data GoDir = Up | Down String

getDir :: LZip -> String -> Maybe GoDir
getDir (cxt, tr) t =
  let chs = map rootLabel $ subForest tr
   in case find (\x -> snd x == t) chs of
        Just ch -> Just (Down t)
        Nothing ->
          if cxt == Hole
            then Nothing
            else
              let (TreeHole k _ _) = cxt
               in if snd k == t
                    then Just Up
                    else Nothing

handleGoto :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> String -> IO ()
handleGoto goGame goMenu z t = do
  let (cxt, tr) = zipper z
  case getDir (cxt, tr) t of
    Nothing -> do
      putStrLn $ "Node " ++ t ++ " not found."
      goGame z
    Just dir -> case dir of
      Up -> case goUp (cxt, tr) of
        Just (ncxt, ntr) ->
          if fst (rootLabel ntr) == Z
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
        Nothing -> do
          putStrLn "You cannot go there."
          goGame z
      Down tt -> case goDown (cxt, tr) tt of
        Just (ncxt, ntr) ->
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
        Nothing -> do
          putStrLn "You cannot go there."
          goGame z