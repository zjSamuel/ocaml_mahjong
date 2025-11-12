(** player.mli — 玩家模块 *)

type t

(*初始化玩家*)
val create : string -> t
(*从玩家对象中获取玩家名字*)
val name : t -> string
(*从玩家对象中获取他的手牌*)
val hand : t -> Hand.t
(*从牌堆中为该玩家摸一张牌*)
val draw_tile : t -> Deck.t -> (t * Deck.t) option
(*打牌*)
val discard_tile : t -> Tile.t -> t option

(** 打印玩家信息与手牌字符串 *)
val to_string : t -> string

(** 判断是否可鸣牌 *)
val can_chi : t -> Tile.t -> bool
val can_pon : t -> Tile.t -> bool
val can_kan : t -> Tile.t -> bool

(** 判定是否可立直或和牌 *)
val can_riichi : t -> bool
val can_tsumo : t -> bool
val can_ron : t -> Tile.t -> bool
