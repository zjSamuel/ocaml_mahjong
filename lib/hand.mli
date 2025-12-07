(** hand.mli — Hand Structure and Helper Operations *)

(** Crucial Fix: Expose the type as a list so other modules can iterate over it *)
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

val tile_to_id : Tile.t -> int
(** Convert a tile to its integer ID (0-33). *)

(** Calculate the Shanten number (minimum tiles needed to complete a winning hand).
    Returns:
    -1 = Winning hand,
     0 = Ready hand (one tile away),
     1 = One tile away from ready hand,
     etc. *)
val calculate_shanten : t -> int

(** Calculate tile efficiency (Ukeire) by determining the number of tiles that improve the hand after discarding each tile.
    Returns a list of [(discarded tile, number of improving tiles)], sorted by efficiency in descending order. *)
val calculate_efficiency : t -> int array -> (Tile.t * int) list

(** Calculate discard recommendations using A* algorithm for Shanten calculation.
    Includes logic for Seven Pairs. 
    Returns a list of [(discarded tile, number of improving tiles)]. *)
val get_recommendations_astar : t -> int array -> (Tile.t * int) list