(* Definition of tile types *)
type suit =
  | Man  (** Characters suit (Manzu) *)
  | Pin  (** Dots suit (Pinzu) *)
  | Sou  (** Bamboo suit (Souzu) *)

type honor =
  | East | South | West | North
  | Red  | Green | White

type t =
  | Numbered of suit * int  (** Numbered tile: suit + number (1–9) *)
  | Honor of honor          (** Honor tile *)

val compare : t -> t -> int
(** Compare the order of two tiles, used for sorting *)

val to_string : t -> string
(** Convert a mahjong tile to a string (e.g., "5Man", "East") *)


