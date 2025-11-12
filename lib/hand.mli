(** hand.mli — 手牌结构与辅助操作 *)

type t = Tile.t list
(** 手牌是一个 Tile 列表 *)

val empty : t
(** 空手牌 *)

val add : t -> Tile.t -> t
(** 添加一张牌并排序 *)

val remove_first : t -> Tile.t -> t option
(** 从手牌中移除第一张匹配的牌 *)

val sort : t -> t
(** 对手牌排序 *)

val to_string : t -> string
(** 将手牌转换为字符串表示 *)

val is_complete : t -> bool
(*判断是否能和*)

val possible_sets : t -> Tile.t list list
(*判断是否能组成搭子*)

