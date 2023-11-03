module Cmd where

data Cmd = Where | LookAround | CheckInventory | UseInventoryWith String | DropInventory | Goto String | Menu
  deriving (Show, Read)

data MenuCmd = SelectAnotherLevel String | Resume | Reset | Quit
  deriving (Show, Read)