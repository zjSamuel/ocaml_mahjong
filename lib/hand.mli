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
