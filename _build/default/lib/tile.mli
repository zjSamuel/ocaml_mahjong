(* 这个是牌类型定义*)
type suit =
  | Man  (** 万子 *)
  | Pin  (** 筒子 *)
  | Sou  (** 索子 *)

type honor =
  | East | South | West | North
  | Red  | Green | White

type t =
  | Numbered of suit * int  (** 数牌：花色 + 数字 (1–9) *)
  | Honor of honor          (** 字牌 *)

val compare : t -> t -> int
(** 比较两张牌的顺序，用于排序 *)

val to_string : t -> string
(** 将麻将牌转换为字符串（例："5万"、"东"） *)

