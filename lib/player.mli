
type difficulty = Easy | Medium | Hard

(** Abstract type representing a player. *)
type t

(** {1 Initialization and Accessors} *)

(** [create name] initializes a new player with the given [name]. *)
val create : string -> t

val name : t -> string

(** Returns the player's current closed hand. *)
val hand : t -> Hand.t

(** Returns the player's discard pile (river). *)
val discards : t -> Tile.t list

(** Returns the tile most recently drawn by the player, if any. *)
val last_drawn : t -> Tile.t option

(** Returns the list of melds (open sets). *)
val melds : t -> Hand.meld list

(** Returns the current difficulty level of the player (for AI). *)
val difficulty : t -> difficulty

(** Sets the difficulty level of the player. *)
val set_difficulty : t -> difficulty -> t

(** [tile_count player]
    Returns the total effective tile count: hand size + (melds * 3). *)
val tile_count : t -> int

(** Checks if the player has a full hand (needs to discard). *)
val has_full_hand : t -> bool

(** {1 Rule Validation} *)

val find_chi_options : t -> Tile.t -> (Tile.t * Tile.t) list
val can_pon : t -> Tile.t -> bool
val can_kan : t -> Tile.t -> bool
val can_ron : t -> Tile.t -> bool
val can_tsumo : t -> bool

(** {1 Core Actions} *)

(** [draw_tile player deck]
    Draws a tile from the deck. Returns updated player and deck. *)
val draw_tile : t -> Deck.t -> (t * Deck.t) option

(** [discard_tile player tile]
    Discards a tile from hand. Returns None if tile not in hand. *)
val discard_tile : t -> Tile.t -> t option

val perform_chi : t -> Tile.t -> Tile.t -> Tile.t -> t option
val perform_pon : t -> Tile.t -> t option
val perform_kan : t -> Tile.t -> t option

(** [add_drawn_tile player tile]
    Directly adds a tile to hand (used for Rinshan draw). *)
val add_drawn_tile : t -> Tile.t -> t

(** {1 AI and Utilities} *)

(** [decide_discard player visible_counts dora_indicators]
    Returns the tile the AI decides to discard based on its difficulty. *)
val decide_discard : t -> int array -> Tile.t list -> Tile.t option

(** [get_recommendations_pure player visible_counts]
    Returns AI suggestions based on pure tile efficiency (Ukeire).
    Format: (tile_to_discard, ukeire_count) list. *)
val get_recommendations_pure : t -> int array -> (Tile.t * int) list

(** [get_recommendations_enhanced player visible_counts dora_indicators]
    Returns AI suggestions based on efficiency + score potential.
    Format: (tile_to_discard, ukeire_count, weighted_score) list. *)
val get_recommendations_enhanced : t -> int array -> Tile.t list -> (Tile.t * int * float) list

val to_string : t -> string
val full_tiles : t -> Tile.t list

(** [Debug Only] Sets the player's hand to a specific configuration. *)
val debug_set_hand : t -> Tile.t list -> t