
type t = Tile.t list
[@@deriving sexp]

type meld =
  | Chi of Tile.t * Tile.t * Tile.t
  | Pon of Tile.t * Tile.t * Tile.t
  | Kan of Tile.t * Tile.t * Tile.t * Tile.t
[@@deriving sexp, compare]

val empty : t
val add : t -> Tile.t -> t
val remove_first : t -> Tile.t -> t option
val sort : t -> t
val to_string : t -> string
val tile_to_id : Tile.t -> int

(** [calculate_shanten hand]
    Returns the minimum shanten number (distance to winning).
    - 0 means "Tenpai" (Ready to win).
    - -1 means "Win" (Complete hand). *)
val calculate_shanten : t -> int

(** [is_complete hand]
    Returns true if the hand is a winning hand (shanten = -1). *)
val is_complete : t -> bool
(** [possible_sets hand]
    Returns all possible groupings of the hand (placeholder in current impl). *)
val possible_sets : t -> Tile.t list list

(** [get_recommendations_astar hand visible_counts]
    Returns a list of (discard_tile, ukeire_count) sorted by efficiency. *)
val calculate_efficiency : t -> int array -> (Tile.t * int) list
val get_recommendations_astar : t -> int array -> (Tile.t * int) list

(** Score calculation module *)
module Score : sig
  type yaku =
    | MenzenTsumo | Riichi | Ippatsu | Pinfu | Tanyao | Iipeiko
    | Yakuhai of string
    | Rinshan | Sanshoku | Itsu | Chanta | Chiitoitsu | Toitoi
    | Sanankou | Sankantsu | SanshokuDoukou | Honroutou | Shousangen
    | Honitsu | Junchan | Ryanpeiko | Chinitsu | Dora of int
  [@@deriving sexp, compare]

  type result = {
    han : int;
    yaku_list : yaku list;
    fu : int;
    points : int
  }
  [@@deriving sexp]
end

(** [calculate_score hand melds dora_indicators round_wind seat_wind is_tsumo is_rinshan]
    Calculates the score of a complete hand. Returns None if hand is not complete or no Yaku. *)
val calculate_score :
  t ->
  meld list ->
  Tile.t list ->
  Tile.honor ->
  Tile.honor ->
  bool ->
  bool ->
  Score.result option