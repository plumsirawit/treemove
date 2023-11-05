# Technical Documentation

The project consists of the main files from the following list:

- `Cmd.hs`: Definition of the `Cmd` and `MenuCmd` data types.
- `Game.hs`: Main game loop. Low-level functionalities are delegated to be handled in `HandlerGame` and `HandlerMenu` instead.
- `GameState.hs`: Definition of the `GameState` data type. This represents the current status of the game.
- `HandlerGame.hs`: Handles _user events_ when playing. This includes game actions, and a special command to bring up the menu.
- `HandlerMenu.hs`: Handles the menu options.
- `Level.hs`: Definition and utilities of a data structure for a level. This includes zipper utilities.
- `LevelSeq.hs`: Handles sequence of levels.
- `Parser.hs`: Parses user input to `Cmd` or `MenuCmd` tokens.

The levels are stored in `Levels/`, where currently (by default) there are five levels.

## Creating your own levels

If you want to create custom levels, you can do so by writing a Haskell file in `Levels/` with the same syntax as the other levels. (The types of nodes are described in `README.md` already.) Then, you can modify the function `loadLevel` from `LevelSeq.hs` (and also don't forget to import `Levels/<NEWLEVEL>.hs (t)` in `LevelSeq.hs`), extending the new level to the list.

The player can use `menu > select level <NEWLEVEL>` to play the custom level.

## Functional Programming Concepts

### First order data types

In `Levels.hs`: `LType`, `LNode`, `LTree`, `LTreeCxt`, `LZip` are data types for the type of a node, the node, the tree, the tree "hole", and the zipper, respectively. In `GameState.hs`, there are `Item` and `GameState` data types (which are self-explanatory). In `Cmd.hs`: `Cmd` and `MenuCmd` (self-explanatory) also. And, in `HandlerGame.hs` there is a `GoDir` helper structure to represent the direction of a neighboring node. Finally, there is a parser type in `Parser.hs`.

### Higher order functions

Each handler in `HandlerGame.hs` and `HandlerMenu.hs` can be thought of as a higher order function which retrieves `goGame` and `goMenu` as continuation callbacks and returns a function from `GameState` to `IO ()`.

### Lambda calculus

Deep concepts from lambda calculus aren't presented much in this project, but lambda expressions are used as a quick way to write anonymous function definitions.

### Side-effects

There are no explicit other side-effects apart from input/output (shown as `IO ()`). As `putStrLn` and `getLine` gives the IO side effects, all code in the handler part are written in IO Monads.

### Laziness

There is no infinite object presented in this project. However, it might be possible to (re)implement the tree zipper to support infinite trees, so that there can be infinitely large levels.

### Zippers

The structure to handle the position of the player inside the tree is a zipper. This is edited from the one given in the BinaryTreeWorld example. Additionally, more functions are written to support operations such as single node mutation from a given tree. This is done in constant time using zippers. (See `LZip` data type in `Level.hs` for explicit usage)
