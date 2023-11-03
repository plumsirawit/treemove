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
handleCheck = undefined -- todo

handleUse :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> String -> IO ()
handleUse = undefined -- todo

handleDrop :: (GameState -> IO ()) -> (GameState -> IO ()) -> GameState -> IO ()
handleDrop = undefined -- todo

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
      putStrLn $ "node " ++ t ++ " not found"
      goGame z
    Just dir -> case dir of
      Up -> case goUp (cxt, tr) of
        Just (ncxt, ntr) ->
          goGame
            ( GameState
                { levelIdent = levelIdent z,
                  initialLevel = initialLevel z,
                  zipper = (ncxt, ntr),
                  movesCount = 1 + movesCount z,
                  bonusCount = bonusCount z
                }
            )
        Nothing -> do
          putStrLn "cannot go there"
          goGame z
      Down tt -> case goDown (cxt, tr) tt of
        Just (ncxt, ntr) ->
          goGame
            ( GameState
                { levelIdent = levelIdent z,
                  initialLevel = initialLevel z,
                  zipper = (ncxt, ntr),
                  movesCount = 1 + movesCount z,
                  bonusCount = bonusCount z
                }
            )
        Nothing -> do
          putStrLn "cannot go there"
          goGame z