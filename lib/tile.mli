(** Definition of tile suits *)
type suit =
  | Man  (** Characters suit (Manzu) *)
  | Pin  (** Dots suit (Pinzu) *)
  | Sou  (** Bamboo suit (Souzu) *)
[@@deriving compare, equal, sexp]

(** Definition of honor tiles *)
type honor =
  | East
  | South
  | West
  | North
  | Red
  | Green
  | White
[@@deriving compare, equal, sexp]

(** Main tile type definition *)
type t =
  | Numbered of suit * int  (** Numbered tile: suit + number (1–9) *)
  | Honor of honor          (** Honor tile *)
[@@deriving compare, equal, sexp]

(** [compare t1 t2]
    Compare the order of two tiles.
    Order: Man (1-9) < Pin (1-9) < Sou (1-9) < East < South < West < North < Red < Green < White. *)
val compare : t -> t -> int

(** [to_string t]
    Convert a mahjong tile to a string representation (e.g., "5Man", "East"). *)
val to_string : t -> string

(** [next_dora t]
    Given a dora indicator, return the actual dora tile.
    Rules:
    - Numbered: n -> n+1 (9 loops back to 1)
    - Winds: East -> South -> West -> North -> East
    - Dragons: White -> Green -> Red -> White *)
val next_dora : t -> t