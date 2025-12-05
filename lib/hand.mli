(** hand.mli — Hand Structure and Helper Operations *)

type t = Tile.t list
(** A hand is represented as a list of tiles. *)

val empty : t
(** An empty hand. *)

val add : t -> Tile.t -> t
(** Add a tile to the hand and sort it. *)

val remove_first : t -> Tile.t -> t option
(** Remove the first matching tile from the hand. *)

val sort : t -> t
(** Sort the hand. *)

val to_string : t -> string
(** Convert the hand into a string representation. *)

val is_complete : t -> bool
(** Determine whether the hand can form a winning hand (ready to win). *)

val possible_sets : t -> Tile.t list list
(** Determine possible combinations (melds or partial sets) that can be formed. *)


val calculate_shanten : t -> int
(** Calculate the Shanten number (minimum tiles needed to complete a winning hand).
    Returns:
    -1 = Winning hand,
     0 = Ready hand (one tile away),
     1 = One tile away from ready hand,
     etc. *)

val calculate_efficiency : t -> (Tile.t * int) list
(** Calculate tile efficiency (Ukeire) by determining the number of tiles that improve the hand after discarding each tile.
    Returns a list of [(discarded tile, number of improving tiles)], sorted by efficiency in descending order. *)