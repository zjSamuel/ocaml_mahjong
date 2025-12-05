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


val calculate_shanten : t -> int
(** 计算向听数 (Shanten)
    返回最小缺牌数：-1=胡牌, 0=听牌, 1=一向听... *)

val calculate_efficiency : t -> (Tile.t * int) list
(** 牌效计算：计算打出每张牌后的进张数 (Ukeire)
    返回列表 [(打出的牌, 进张数)]，按效率降序排列 *)