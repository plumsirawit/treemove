module Cmd where

data Cmd = Where | LookAround | CheckInventory | UseInventoryWith String | DropInventory | PickInventory | Goto String | Menu | HelpGame
  deriving (Show, Read)

data MenuCmd = SelectAnotherLevel String | Resume | Reset | Quit | HelpMenu
  deriving (Show, Read)