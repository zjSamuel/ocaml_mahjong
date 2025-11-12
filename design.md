## I. Core UI Layout and Demo Flow

This section details the primary user interface and the step-by-step user flow intended for the demo.

![UI Screenshot](./UI.png)


### A. Main Game Board Layout

The board is presented from the perspective of the human player (South).

* **Player Positions:**
    * **South (You):** Bottom of the screen. Hand tiles are face-up.
    * **North, East, West (AI):** Top, right, and left. Hand tiles are face-down.
* **Player Area (South):**
    * Displays the 13-tile hand.
    * Action Buttons: `Sort Hand`, `Discard`, and `Win (Hu)` are located above the hand.
    * Card Values: There is a value below each tile, which are generated from our searching algorithm.
* **Center Area:**
    * **Wall:** Shows the count of remaining tiles (e.g., "Remaining: 12").
    * **Discard Piles:** Each player's discards are placed in the center, forming a cross layout.
* **Dora Area:**
    * Located in the top-left, showing five tiles, with one (the indicator) flipped face-up.
* **Tile Suits:**
    * **Characters (W)**
    * **Dots (T)**
    * **Bamboos (S)**
    * **Honors (Charcter)**



### B. Core Action Flow  and Demo Sequence

This flow describes the player's turn from start to finish.

**1. Initial State:**
* All players have 13 tiles.
* The Wall shows "Remaining: 12".
* The `Discard` and `Win (Hu)` buttons are **disabled**.

**2. Player Draws a Tile:**
* **Trigger:** The player's turn begins.
* **Action:** The system automatically draws one tile from the Wall.
* **UI Update:**
    * The player's hand temporarily shows 14 tiles (the new tile is highlighted).
    * The Wall count decreases to "Remaining: 11".
    * The `Discard` and `Win (Hu)` buttons become **active**.

**3. Player Discards a Tile:**
* **Trigger:** The player clicks a tile from their 14-tile hand.
* **Action:** The selected tile is moved to the South discard pile.
* **UI Update:** The hand returns to 13 tiles.

**4. Player Sorts Hand (Optional):**
* **Trigger:** The player clicks the `Sort Hand` button.
* **Action:** The hand tiles are re-ordered by suit (W, T, S, Honors).

### C. Win Modal

This screen appears when the player successfully declares a win.

![UI Screenshot](./WIN.png)

* **Trigger:** Player clicks the `Win (Hu)` button when a winning condition is met.
* **UI Update:**
    * A modal window appears in the center of the screen.
    * **Title:** "WIN"
    * **Content:** The player's full 14-tile winning hand is displayed, grouped into sets (melds) and the pair.
    * **Action:** A `Continue` button is present to proceed to the next round or end the game.

