(** deck.mli — Deck Module *)

type t

val create : unit -> t
(** Generate and shuffle a new wall. *)

val draw : t -> (Tile.t * t) option
(** Draw one tile from the top of the wall. *)

val remaining : t -> int
(** Get the number of remaining tiles in the current wall. *)

val get_dora_indicators : t -> Tile.t list
(** Get list of currently visible dora indicators *)

val add_dora_indicator : t -> t
(** Reveal a new dora indicator (called after Kan) *)

val draw_rinshan : t -> (Tile.t * t) option
(** Draw a Rinshan tile (from the dead wall) *)

val debug_force_next : t -> Tile.t -> t
