module Parser where

import Cmd
import Control.Applicative

newtype Parser tok a = Parser {runParser :: [tok] -> Maybe (a, [tok])}

instance Monad (Parser tok) where
  -- return :: a -> Parser tok a
  return x = Parser (\ts -> Just (x, ts))

  -- (>>=) :: Parser a -> (a -> Parser tok b) -> Parser tok b
  p >>= f =
    Parser
      ( \ts -> case runParser p ts of
          Nothing -> Nothing
          Just (x, ts') -> runParser (f x) ts'
      )

instance Functor (Parser tok) where
  fmap f p = p >>= \x -> return (f x)

instance Applicative (Parser tok) where
  pure = return
  pf <*> p = pf >>= \f -> p >>= \x -> return (f x)

instance Alternative (Parser tok) where
  -- empty :: Parser tok a
  empty = Parser (\ts -> Nothing)

  -- (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
  p1 <|> p2 =
    Parser
      ( \ts -> case runParser p1 ts of
          Just (x, ts') -> Just (x, ts')
          Nothing -> runParser p2 ts
      )

token :: Parser tok tok
token = Parser $ \ts -> case ts of
  [] -> Nothing
  (t : ts') -> Just (t, ts')

sat :: (tok -> Bool) -> Parser tok tok
sat p = do
  t <- token
  if p t then return t else empty

match :: String -> Parser String String
match s = sat (== s) -- case sensitive

label :: Parser String String
label = sat (const True) -- get any string

parseCmd :: Parser String Cmd
parseCmd = parseWhere <|> parseLook <|> parseCheck <|> parseUse <|> parseDrop <|> parseGoto <|> parseMenu

parseWhere :: Parser String Cmd
parseWhere = do
  match "where"
  return Where

parseLook :: Parser String Cmd
parseLook = do
  match "look"
  match "around"
  return LookAround

parseCheck :: Parser String Cmd
parseCheck = do
  match "check"
  match "inventory"
  return CheckInventory

parseUse :: Parser String Cmd
parseUse = do
  match "use"
  match "inventory"
  match "with"
  n <- label
  return $ UseInventoryWith n

-- todo: import context of the level to the parser, to check whether the label is a valid node or not.
-- (or should we handle that outside the parser -- the functionality is actually irrelevant to parsing)

parseDrop :: Parser String Cmd
parseDrop = do
  match "drop"
  match "inventory"
  return DropInventory

parseGoto :: Parser String Cmd
parseGoto = do
  match "go"
  match "to"
  n <- label
  return $ Goto n

parseMenu :: Parser String Cmd
parseMenu = do
  match "menu"
  return Menu

parseMenuCmd :: Parser String MenuCmd
parseMenuCmd = parseSelect <|> parseResume <|> parseReset <|> parseQuit

parseSelect :: Parser String MenuCmd
parseSelect = do
  match "select"
  match "level"
  n <- label
  return $ SelectAnotherLevel n

parseResume :: Parser String MenuCmd
parseResume = do
  match "resume"
  return Resume

parseReset :: Parser String MenuCmd
parseReset = do
  match "reset"
  return Reset

parseQuit :: Parser String MenuCmd
parseQuit = do
  match "quit" <|> match "q"
  return Quit

parseInput :: MonadFail m => Parser String a -> String -> m a
parseInput p s = case runParser p (words s) of
  Just (x, ts') -> if null ts' then return x else fail "runParserInput: some tokens left"
  Nothing -> fail "runParserInput: failed to parse"