# treemove

CSE301 Functional Programming Project

## Express Usage

If you're using `ghc`, you can `ghc Game.hs -o Game` and then `./Game` directly.

## About this game

Treemove is a text-based single-player puzzle-adventure game (currently) composed of 4 levels. It is inspired by [Sokoban](https://en.wikipedia.org/wiki/Sokoban), (heavily from) Zeek the Geek and (heavily from) [Wonderland Secret Worlds](https://www.midnightsynergy.com/secretworlds/). In each level, the game map is a tree where the player initially lives inside a node (starting node, `S`) of the tree. The goal of the level is for the player to reach the exit node (`F`).

Following is the list of types of nodes.

- `S`: Starting node.
- `F`: Exit node.
- `E`: Empty node.
- `B`: Box node. The player can enter the box node, and if the inventory is empty, the player can _carry_ the box. This will change the inventory of the player from `nothing` (empty) to `a box` and change the node from `B` (box node) to `E` (empty node). In any case, the player can just walk past the box without carrying it.
- `W`: Water node. The player cannot enter a water node until the water has been filled with a box. This means the player can choose, if the inventory is `a box`, to place the box on the water, thus making a bridge. This will change the inventory back from `a box` to `nothing` and change the node from `W` to `E`.
- `Key x`: Key node. The player can enter and exit key nodes freely as if it is an empty node. But, in addition to that, if the inventory is currently empty, the player can choose to pick up the key. This will change the node from `Key x` to `E` and change the inventory from `nothing` to `the key x`, where `x` is a character representing the type of the key.
- `Gate x`: Gate node. The player cannot enter the node, but if the inventory is `the key x`, then the player can choose to use the key to unlock the gate. This will change the node from `the key x` to `nothing`.
- `Z`: Bonus node. The player gains a bonus coin once the player enters this node. Note that the bonus coins will not be in the inventory, but counts towards the bonus count (another global counter) of the level. After entering this node, it will be transformed into `E`.

## Controls

You can type `help` to get the list of available controls, or see the following list.

- `where`: Ask for your current position.
- `look around`: See types and names of the neighboring nodes.
- `check inventory`: Check your current inventory.
- `use inventory with L`: Use your current inventory with `L`. Note that `L` has to be a neighboring node.
- `drop inventory`: Drop your current item from your inventory. Note that the current node has to be empty.
- `pick inventory`: Pick up the item in the current node. Note that the current node has to be empty.
- `go to L`: Move to `L`. Note that `L` has to be a neighboring node.
- `menu`: Show the game menu.
- `help`: Show the list of available controls.

## Menu

Once inside the menu, you can type `help` to get the list of menu options, or see the following list.

- `select level T`: Change the current level to `T`.
- `resume`: Resume the game.
- `reset`: Reset the current level.
- `quit`: Leave the game.
- `help`: Show the list of available menu options.
