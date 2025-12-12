(** lib/game.mli *)

type t
(** Abstract type representing the game state. *)

(** {1 Initialization} *)

val create : unit -> t
(** [create ()] Initializes a new game.
    - Creates and shuffles the deck.
    - Deals 13 tiles to each of the 4 players.
    - Sets Player 0 (usually Human) as the starting player. *)

(** {1 State Accessors} *)

val current_player : t -> Player.t
(** Returns the player whose turn it currently is. *)

val current_player_id : t -> int
(** Returns the index (0-3) of the current player. *)

val remaining_tiles : t -> int
(** Returns the number of tiles remaining in the live wall. *)

val all_players : t -> Player.t list
(** Returns all players (useful for UI rendering). *)

val last_discard : t -> Tile.t option
(** Returns the most recently discarded tile (for Chi/Pon/Ron checks). *)

val get_dora_indicators : t -> Tile.t list
(** Returns the list of visible dora indicator tiles. *)

val get_visible_counts : t -> int -> int array
(** [get_visible_counts game viewer_idx] Returns an array (size 34) counting all
    visible tiles from the perspective of [viewer_idx]. This includes:
    - All discards on the table.
    - All open melds (Chi/Pon/Kan) of all players.
    - The viewer's own hand. Used by AI to calculate tile probability (Ukeire).
*)

(** {1 Core Game Loop Actions} *)

val draw_card : t -> t * Tile.t option
(** [draw_card game] Current player draws a tile. Returns (new_game_state,
    drawn_card_option). *)

val discard_card : t -> Tile.t -> t * Tile.t option
(** [discard_card game tile] Current player discards a tile. Returns
    (new_game_state, discarded_tile_option). *)

val next_turn : t -> t
(** [next_turn game] Advances the turn to the next player (0->1->2->3->0). *)

(** {1 Interactions (Melds)} *)

val perform_chi : t -> Tile.t -> Tile.t -> Tile.t -> t * bool
val perform_pon : t -> Tile.t -> t * bool
val perform_kan : t -> Tile.t -> t * bool

(** {1 State Checks} *)

val is_over : t -> bool
val winner : t -> Player.t option
val can_current_player_discard : t -> bool
val can_current_player_draw : t -> bool

(** {1 Bot Automation} *)

val play_bot_step : t -> t * bool
(** [play_bot_step game] Executes a single step for the current bot player (Draw
    -> AI Think -> Discard). Returns (new_game_state, did_something_bool). *)

val set_bot_difficulty : t -> int -> Player.difficulty -> t
(** [set_bot_difficulty game player_idx difficulty] Sets the AI level for a
    specific player. *)

(** {1 Debugging} *)

val debug_set_player : t -> int -> Player.t -> t
(** [debug_set_player game idx player] Replaces a player state directly. *)
