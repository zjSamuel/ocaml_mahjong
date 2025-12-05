(** player.mli — Player module
    Manages the state of an individual player, including their hand, discard pile,
    melds (open sets), and rule validation logic.
*)

type t
(** Abstract type representing a player.
    Contains the player's name, current hand, discard history, and meld status. *)

type meld = 
  | Chi of Tile.t * Tile.t * Tile.t
  | Pon of Tile.t * Tile.t * Tile.t
  | Kan of Tile.t * Tile.t * Tile.t * Tile.t
(** Represents a meld (open set/naki) of tiles:
    - [Chi (t1, t2, target)]: A sequence of three consecutive tiles (Chow).
    - [Pon (target, t, t)]: Three identical tiles (Pung).
    - [Kan (target, t, t, t)]: Four identical tiles (Kong, specifically Daiminkan). *)

(** {1 Initialization and Accessors} *)

val create : string -> t
(** [create name] initializes a new player with the given [name].
    The player starts with an empty hand and empty discard pile. *)

val name : t -> string
(** Returns the player's name. *)

val hand : t -> Hand.t
(** Returns the player's current closed hand (standing tiles).
    Note: This does not include tiles that have been melded. *)

val discards : t -> Tile.t list
(** Returns the player's discard pile (river).
    The list is ordered with the most recently discarded tile at the head. *)

val last_drawn : t -> Tile.t option
(** Returns the tile most recently drawn by the player, if any.
    This is used for UI highlighting and to determine "Tsumogiri" (discarding the drawn tile). *)

val melds : t -> meld list
(** Returns the list of melds (open sets) the player has made. *)

val tile_count : t -> int
(** Calculates the effective tile count of the player.
    Formula: [count(hand) + (count(melds) * 3)].
    Used to determine if the player is in the "discard phase" (14 tiles) or "draw phase" (13 tiles). *)

val has_full_hand : t -> bool
(** Checks if the player has a full hand (needs to discard).
    Equivalent to [tile_count p >= 14]. *)

(** {1 Rule Validation (Predicates)} *)

val find_chi_options : t -> Tile.t -> (Tile.t * Tile.t) list
(** Finds all possible pairs in the hand that can form a sequence (Chi) with the [target] tile.
    - [target]: The tile discarded by the player to the left (Kamicha).
    - Returns: A list of valid [(t1, t2)] pairs such that [t1, t2, target] form a valid sequence. *)

val can_pon : t -> Tile.t -> bool
(** Checks if the player can perform a "Pon" (Pung) on the [target] tile.
    Returns [true] if the player holds at least 2 tiles identical to [target]. *)

val can_kan : t -> Tile.t -> bool
(** Checks if the player can perform a "Kan" (Kong) on the [target] tile.
    Returns [true] if the player holds at least 3 tiles identical to [target]. *)

val can_ron : t -> Tile.t -> bool
(** Checks if the player can declare "Ron" (win on discard) on the [target] tile.
    Validates if [hand + target] forms a complete winning hand (4 melds + 1 pair). *)

val can_tsumo : t -> bool
(** Checks if the player can declare "Tsumo" (win on self-draw).
    Validates if the current hand (which already includes the drawn tile) is a winning hand. *)

(** {1 Core Actions} *)

val draw_tile : t -> Deck.t -> (t * Deck.t) option
(** Draws a tile from the deck for this player.
    - Returns [Some (updated_player, updated_deck)] on success.
    - Returns [None] if the deck is empty. *)

val discard_tile : t -> Tile.t -> t option
(** Discards a specific [tile] from the player's hand.
    - Removes the tile from the hand and adds it to the discard pile.
    - Resets [last_drawn] to [None].
    - Returns [None] if the player does not actually hold the specified tile. *)

val perform_chi : t -> Tile.t -> Tile.t -> Tile.t -> t option
(** Executes a "Chi" action.
    - [target]: The tile being claimed.
    - [t1], [t2]: The matching tiles from the player's hand.
    - Returns an updated player with tiles moved from hand to [melds]. *)

val perform_pon : t -> Tile.t -> t option
(** Executes a "Pon" action.
    - [target]: The tile being claimed.
    - Automatically removes two matching tiles from the hand and creates a [Pon] meld. *)

val perform_kan : t -> Tile.t -> t option
(** Executes a "Kan" action.
    - [target]: The tile being claimed.
    - Automatically removes three matching tiles from the hand and creates a [Kan] meld.
    - Note: This function only updates the hand structure. The logic for drawing a Rinshan tile is handled in the [Game] module. *)

(** {1 AI and Utilities} *)

val get_recommendations : t -> (Tile.t * int) list
(** Calculates tile efficiency to suggest the best discard.
    - Only valid when the player has a full hand (14 tiles).
    - Returns a list of [(tile_to_discard, ukeire_count)], sorted by efficiency (descending). *)

val to_string : t -> string
(** Returns a string representation of the player (name, hand, melds) for debugging purposes. *)

val debug_set_hand : t -> Tile.t list -> t
(** [Debug Only] Forcibly sets the player's hand to a specific list of tiles.
    Used for testing winning conditions or specific scenarios. *)