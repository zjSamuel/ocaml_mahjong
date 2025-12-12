(** Abstract type representing the deck (wall) state. *)
type t

(** [create ()]
    Generates a new, shuffled deck.
    - Total tiles: 136 (34 types * 4 copies).
    - Reserves 14 tiles for the Dead Wall (Wangpai). *)
val create : unit -> t

(** [draw deck]
    Draws one tile from the top of the live wall.
    @return [Some (tile, new_deck)] if tiles remain.
    @return [None] if the wall is empty. *)
val draw : t -> (Tile.t * t) option

(** [remaining deck]
    Returns the count of draw-able tiles in the live wall. *)
val remaining : t -> int

(** [get_dora_indicators deck]
    Returns the list of currently visible dora indicator tiles from the Dead Wall. *)
val get_dora_indicators : t -> Tile.t list

(** [add_dora_indicator deck]
    Reveals a new dora indicator (used after a Kan/Kong).
    Max limit: 5 indicators. *)
val add_dora_indicator : t -> t

(** [draw_rinshan deck]
    Draws a replacement tile from the Dead Wall (Rinshan pai).
    Note: To maintain the 14-tile dead wall rule, this usually involves
    moving the last tile of the live wall to the dead wall, though simplified here. *)
val draw_rinshan : t -> (Tile.t * t) option

(** [debug_force_next deck tile]
    [Debug Only] Forces the next drawn tile to be [tile] by placing it at the top of the wall. *)
val debug_force_next : t -> Tile.t -> t