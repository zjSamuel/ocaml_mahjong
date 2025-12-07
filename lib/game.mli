(** game.mli — Game Flow Control Module Responsible for maintaining and
    operating the overall game state, including dealing, drawing, discarding,
    and turn rotation logic. *)

type t
(** Abstract type representing the game state. The implementation includes:
    - [deck]: the current wall (Deck.t)
    - [players]: the four players (Player.t array)
    - [discard_pile]: the discard pile
    - [current_player_idx]: the index of the current player (0–3) *)

(** {1 Initialization and State Access} *)

val create : unit -> t
(** Create a new game instance.
    - Automatically generates and shuffles the deck;
    - Deals 13 tiles to each player;
    - Sets the current player to player 0 by default. *)

val current_player : t -> Player.t
(** Get the player whose turn it currently is. *)

(** {2 Core Game Flow Functions} *)

val draw_card : t -> t * Tile.t option
(** The current player draws a tile from the wall. Returns:
    - The updated game state;
    - [Some tile] if a tile was successfully drawn;
    - [None] if the wall is empty. *)

val discard_card : t -> Tile.t -> t * Tile.t option
(** The current player discards a specified tile, updating the discard pile and
    rotating to the next player. Returns:
    - The updated game state;
    - [Some tile] if the discard was successful;
    - [None] if the tile was not in the player's hand. *)

val next_turn : t -> t
(** Switch the current player to the next one (0 → 1 → 2 → 3 → 0). *)

(** {3 Game State Checks} *)

val is_over : t -> bool
(** Determine whether the game has ended (e.g., the wall is empty or a player
    has won). *)

val winner : t -> Player.t option
(** Return the winning player, if any. Currently a placeholder interface—future
    implementations may include win detection logic. *)

val remaining_tiles : t -> int
(** Returns the number of tiles remaining in the live wall (draw pile). *)

val all_players : t -> Player.t list
(** Returns a list of all player objects (indices 0 to 3). Useful for iterating
    over players to render the UI or debug info. *)

val last_discard : t -> Tile.t option
(** Returns the most recently discarded tile, if any. This is used to determine
    if the main player can claim the tile (Chi/Pon/Kan/Ron). *)

(** {4 Interaction and Interruptions (Ming-pai)} *)

val perform_chi : t -> Tile.t -> Tile.t -> Tile.t -> t * bool
(** Attempts to perform a "Chi" (Chow) action for the human player (Player 0).
    Arguments:
    - [target]: The discarded tile being claimed.
    - [t1], [t2]: The two tiles from the player's hand that form the sequence
      with [target]. Returns:
    - [(new_game_state, true)] if the action is valid and successful.
    - [(original_state, false)] if the action is invalid. *)

val perform_pon : t -> Tile.t -> t * bool
(** Attempts to perform a "Pon" (Pung) action for the human player (Player 0).
    Arguments:
    - [target]: The discarded tile being claimed. Returns
      [(new_game_state, true)] if the player has a matching pair and the action
      succeeds. *)

val perform_kan : t -> Tile.t -> t * bool
(** Attempts to perform a "Kan" (Kong, specifically Daiminkan) for the human
    player (Player 0). Arguments:
    - [target]: The discarded tile being claimed. Returns
      [(new_game_state, true)] if successful. Side effects: Removes 3 matching
      tiles from hand, creates a Meld, and automatically draws a Rinshan
      replacement tile. *)

(** {5 Phase Checks} *)

val can_current_player_discard : t -> bool
(** Checks if the current player is in the "discard phase". Returns [true] if
    the player has a full hand (effective count >= 14) and must discard. *)

val can_current_player_draw : t -> bool
(** Checks if the current player is in the "draw phase". Returns [true] if the
    player has fewer than 14 tiles and is allowed to draw. *)

(** {6 Bot Automation} *)

val play_bot_step : t -> t * bool
(** Executes a single action step (Draw -> Discard) for the current bot player.
    Returns:
    - [(new_state, true)] if the bot successfully played a turn.
    - [(state, false)] if it is currently the human's turn or the deck is empty.
*)

(** {7 Utilities} *)

val current_player_id : t -> int
(** Returns the integer index (0–3) of the player whose turn it currently is. *)

val debug_set_player : t -> int -> Player.t -> t
(* just for debug to set player state *)

val get_visible_counts : t -> int -> int array
val get_dora_indicators : t -> Tile.t list
val set_bot_difficulty : t -> int -> Player.difficulty -> t
