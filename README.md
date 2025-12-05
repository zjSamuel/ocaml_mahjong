# ocaml_mahjong

Zeli Ma, Jiewen Luo, Jin Zhou

Japanese mahjong implemented with ocaml, for FPSE 2025Fall


## Project Overview

This project aims to develop a comprehensive Japanese Mahjong application using OCaml. The primary goal is to build a game engine that correctly implements the complex rules of Riichi Mahjong.

A core feature of this project is an advanced AI and analysis module. This module will be powered by an A* search algorithm designed to evaluate hand efficiency (i.e., *shanten*, or steps to a winning hand) and determine the optimal discard choice in any given situation.

The final application will serve multiple purposes:

* **AI Opponents:** Different difficult levels AI to play against.

* **A Training Tool:** A system to analyze player decisions and provide optimal play recommendations.


## How to play

* `dune exec bin/main.exe` to start, run on localhost:3000
* Click cards to display, click operation buttons to perform `chi`, `pon`, `kan` `tsumo` and `ron`
* Play with 3 AI-bots who can only draw and play what they just draw like a dummy 
![alt text](images/image.png)
![alt text](images/image-1.png)
* Basic instruction on "AI assistant" to tell you what is the best choices of discard
![alt text](images/image-2.png)

## Finished functions

* Basic structure including fundamental functions: `draw`, `play` (discard), and `win` (basic win check) and operations: `chi`, `pon`, `kan` and `ron`.
* Basic frontend interface written with `HTML` and `Dream`
* Basic algorithm to calculate "Shanten" and tile efficiency, which give player recommendation on what to discard by showing the contribution to Shanten

## Implementation

### Backend

* All game logic is encapsulated in the `Mahjong` library, ensuring it is reusable and testable without the frontend.
* **tile** defines basic type of Mahjong cards and implement operations like `compare` and `to_string` for usage
* **deck** defines the deck, where player draw cards. It handles create, shuffle and draw operations
* **hand** implements the functions of cards in one player's hand. It also calculate `shanten` (distance to build a hand which can win) and the efficiency of each card, which can give instruction to player to play the best card. It is implemented by traversing all mahjong card types and performing a recursive depth-first search (DFS) to find possible combinations.
  * **Algorithm**:
      The core of our AI and assistance system is a **recursive backtracking algorithm** that calculates *shanten* (minimum moves to win) and *ukeire* (tile acceptance). The process follows these steps:

      1.  **Shanten Calculation (Standard Form)**:
          * The algorithm converts the hand into a **frequency table** (array of tile counts).
          * It first attempts to identify a pair (the "head").
          * It then performs a **Depth-First Search (DFS)** to extract all possible *melds* (sequences or triplets).
          * After extracting melds, a **greedy strategy** counts the remaining *tatsu* (incomplete sets like `2-3` or `2-2`).
          * The final shanten value is derived using the standard formula: $8 - (2 \times \text{Melds} + \text{Tatsu} + \text{Pair})$.
          * It simultaneously calculates the shanten for a headless hand (single wait) and takes the minimum.

      2.  **Ukeire (Effective Tile) Calculation**:
          * Given a 13-tile hand, the system iterates through all 34 unique Mahjong tiles.
          * For each tile, it simulates drawing it and recalculating the shanten.
          * If the shanten **decreases**, the tile is considered "effective."
          * The total count of effective tiles is summed (assuming 4 of each exist, minus visible ones in hand).

      3.  **Efficiency Ranking**:
          * For a 14-tile hand, the system iterates through every unique tile in the hand.
          * It simulates discarding each tile and calculates the resulting *ukeire* of the remaining 13 tiles.
          * Discards are ranked by this count, recommending moves that maximize the probability of advancing the hand.
  * Apply the algorithm to have the same performance like https://mj.fyisvia.com/discard and test.py
* **Player** defines the player type with hands and melds(the result of `chi` `pon` `kan`)and the operations player can do, including fundamental functions: `draw`, `play` (discard), and `win` (basic win check) and operations: `chi`, `pon`, `kan` and `ron`.
* **Game** is the core controller of the whole project. It control the turn change of 4 players, and manage the behavior of player and deck.

### Frontend

* **bin/main** : Write HTML to call functions from backend. The `main` file manages the creation of a game, the connection between `HTML` frontend and `Ocaml` backend(by `HTML` action and `Dream` routes ), and all the behavior of game.

## TODO

* Implement complex rules on score calculation. Current version can only let people know they win or not.
* Implement better algorithms (like A*) to calculate card efficiency and generate instruction. Current version use Brute-force algorithm and only consider player's hand.
  * Apply the algorithm on AI-bots, who can play much better and even easily win human players.
  
## Tests

* We already implement tests on our lib, and get overall coverage more than 90%.
![alt text](images/78b5b6a43dcf3bf2ca8b15b0aaaf19ff.png)