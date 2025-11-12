(** player.mli — Player module *)

type t

(* Initialize a player *)
val create : string -> t
(* Get the player's name from the player object *)
val name : t -> string
(* Get the player's hand from the player object *)
val hand : t -> Hand.t
(* Draw a tile from the deck for this player *)
val draw_tile : t -> Deck.t -> (t * Deck.t) option
(* Discard a tile *)
val discard_tile : t -> Tile.t -> t option

(** Print player information and hand as a string *)
val to_string : t -> string

(** Determine if the player can call a meld *)
val can_chi : t -> Tile.t -> bool
val can_pon : t -> Tile.t -> bool
val can_kan : t -> Tile.t -> bool

(** Determine if the player can declare riichi or win *)
val can_riichi : t -> bool
val can_tsumo : t -> bool
val can_ron : t -> Tile.t -> bool

