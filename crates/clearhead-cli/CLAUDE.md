# Project Overview
You can read the proper overview at [the README](READEME.md) but for a quick overview

We are trying to make a cli to work in compliment with [my custom treesitter format](https://github.com/ClearHeadToDo-Devs/tree-sitter-actions)

We are still in the early stages where we are trying to figure out a nice way to translate the AST into something useful for:
- data manipulation
- querying
- updates that can span multiple nodes
- and even running the eventual LSP server that can power IDEs

to do all of that, we need a solid way to put the tree into a proper format
