(** deck.mli — Deck Module *)

type t

val create : unit -> t
(** Generate and shuffle a new wall. *)

val draw : t -> (Tile.t * t) option
(** Draw one tile from the top of the wall. *)

val remaining : t -> int
(** Get the number of remaining tiles in the current wall. *)

(** Get list of currently visible dora indicators *)
val get_dora_indicators : t -> Tile.t list

(** Reveal a new dora indicator (called after Kan) *)
val add_dora_indicator : t -> t

(** Draw a Rinshan tile (from the dead wall) *)
val draw_rinshan : t -> (Tile.t * t) option
