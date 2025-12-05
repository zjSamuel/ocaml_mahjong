(* lib/player.mli *)
type t
type meld = 
  | Chi of Tile.t * Tile.t * Tile.t
  | Pon of Tile.t * Tile.t * Tile.t
  | Kan of Tile.t * Tile.t * Tile.t * Tile.t

val create : string -> t
val name : t -> string
val hand : t -> Hand.t
val discards : t -> Tile.t list
val last_drawn : t -> Tile.t option
val melds : t -> meld list

(* 判定接口 *)
val find_chi_options : t -> Tile.t -> (Tile.t * Tile.t) list
val can_pon : t -> Tile.t -> bool
val can_kan : t -> Tile.t -> bool
val can_ron : t -> Tile.t -> bool
val can_tsumo : t -> bool

(* 执行接口 *)
val draw_tile : t -> Deck.t -> (t * Deck.t) option
val discard_tile : t -> Tile.t -> t option
val perform_chi : t -> Tile.t -> Tile.t -> Tile.t -> t option
val perform_pon : t -> Tile.t -> t option
val perform_kan : t -> Tile.t -> t option

val to_string : t -> string
val debug_set_hand : t -> Tile.t list -> t

(** 获取玩家当前有效牌数（手牌 + 副露x3） *)
val tile_count : t -> int

(** 判断玩家是否满手牌（需要打牌） *)
val has_full_hand : t -> bool

(* 添加接口 *)
(* val get_recommendations : t -> (Tile.t * int) list *)