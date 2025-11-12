(** wall.mli — 牌山模块 *)

type t

val create_full : unit -> t
(** 生成完整的一副麻将牌（136 张），包括万、筒、索与字牌各 4 张。 *)

val draw : t -> (Tile.t * t) option
(** 从牌山顶端摸一张牌。
    返回 [(摸到的牌, 更新后的牌山)]。
    若牌山为空，返回 [None]。 *)

val draw_n : t -> int -> Tile.t list -> (Tile.t list * t)
(** 连续摸 N 张牌，用于发牌阶段。
    参数为 [当前牌山] [要摸的张数] [已摸的列表]；
    返回 [(摸到的牌列表, 更新后的牌山)]。 *)

val shuffle : t -> t
(** 将整副牌洗乱，返回新牌山。 *)

val create : unit -> t
(** 生成并洗好的一副新牌堆 *)

val remaining : t -> int
(** 获取当前牌山剩余的张数。 *)


(** 宝牌相关 *)
val dora_indicator : t -> Tile.t
val next_dora_indicator : t -> t
