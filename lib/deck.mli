(** deck.mli — Deck Module *)

type t

val create_full : unit -> t
(** Generate a complete Mahjong tile set (136 tiles),
    including four copies of each Man, Pin, Sou, and honor tile. *)

val draw : t -> (Tile.t * t) option
(** Draw one tile from the top of the wall.
    Returns [(drawn tile, updated wall)].
    Returns [None] if the wall is empty. *)

val draw_n : t -> int -> Tile.t list -> (Tile.t list * t)
(** Draw N consecutive tiles, used during the dealing phase.
    Parameters: [current wall] [number of tiles to draw] [accumulated list];
    Returns [(list of drawn tiles, updated wall)]. *)

val shuffle : t -> t
(** Shuffle the entire wall and return the new shuffled wall. *)

val create : unit -> t
(** Generate and shuffle a new wall. *)

val remaining : t -> int
(** Get the number of remaining tiles in the current wall. *)


(** Dora-related functions *)
val dora_indicator : t -> Tile.t
val next_dora_indicator : t -> t

