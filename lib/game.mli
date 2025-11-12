(** game.mli — Game Flow Control Module
    Responsible for maintaining and operating the overall game state,
    including dealing, drawing, discarding, and turn rotation logic.
*)

type t
(** Abstract type representing the game state.
    The implementation includes:
    - [deck]: the current wall (Deck.t)
    - [players]: the four players (Player.t array)
    - [discard_pile]: the discard pile
    - [current_player_idx]: the index of the current player (0–3)
*)

(** {1 Initialization and State Access} *)

val create : unit -> t
(** Create a new game instance.
    - Automatically generates and shuffles the deck;
    - Deals 13 tiles to each player;
    - Sets the current player to player 0 by default. *)

val current_player : t -> Player.t
(** Get the player whose turn it currently is. *)

val to_string : t -> string
(** Convert the entire game state to a string, including:
    - Each player's hand
    - The discard pile
    - The number of remaining tiles
    - Current player information *)

(** {1 Core Game Flow Functions} *)

val draw_card : t -> (t * Tile.t option)
(** The current player draws a tile from the wall.
    Returns:
    - The updated game state;
    - [Some tile] if a tile was successfully drawn;
    - [None] if the wall is empty.
*)

val discard_card : t -> Tile.t -> (t * Tile.t option)
(** The current player discards a specified tile, updating the discard pile
    and rotating to the next player.
    Returns:
    - The updated game state;
    - [Some tile] if the discard was successful;
    - [None] if the tile was not in the player's hand.
*)

val next_turn : t -> t
(** Switch the current player to the next one (0 → 1 → 2 → 3 → 0). *)

val play_turn : t -> t
(** Execute a full turn (draw → discard), useful for automated mode or testing. *)

(** {1 Game State Checks} *)

val is_over : t -> bool
(** Determine whether the game has ended
    (e.g., the wall is empty or a player has won). *)

val winner : t -> Player.t option
(** Return the winning player, if any.
    Currently a placeholder interface—future implementations may include win detection logic. *)
