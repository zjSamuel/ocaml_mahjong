open Core
[@@@coverage off]
type suit =
  | Man (** Characters *)
  | Pin (** Dots *)
  | Sou (** Bamboo *)
[@@deriving compare, equal, sexp]

type honor =
  | East
  | South
  | West
  | North
  | Red
  | Green
  | White
[@@deriving compare, equal, sexp]

type t =
  | Numbered of suit * int
  | Honor of honor
[@@deriving compare, equal, sexp]

[@@@coverage on]
let suit_to_int = function
  | Man -> 0
  | Pin -> 1
  | Sou -> 2

let honor_to_int = function
  | East -> 0
  | South -> 1
  | West -> 2
  | North -> 3
  | Red -> 4
  | Green -> 5
  | White -> 6

let compare t1 t2 =
  match (t1, t2) with
  | Numbered (s1, n1), Numbered (s2, n2) ->
      let sc = Int.compare (suit_to_int s1) (suit_to_int s2) in
      if sc <> 0 then sc
      else
        Int.compare n1 n2
  | Honor h1, Honor h2 ->
      Int.compare (honor_to_int h1) (honor_to_int h2)
  | Numbered _, Honor _ -> -1
  | Honor _, Numbered _ -> 1

let to_string = function
  | Numbered (suit, n) ->
      let s = match suit with Man -> "万" | Pin -> "筒" | Sou -> "索" in
      Printf.sprintf "%d%s" n s
  | Honor h -> (
      match h with
      | East -> "东"
      | South -> "南"
      | West -> "西"
      | North -> "北"
      | Red -> "中"
      | Green -> "发"
      | White -> "白")

let next_dora = function
  | Numbered (suit, n) ->
      if n = 9 then Numbered (suit, 1) else Numbered (suit, n + 1)
  | Honor h ->
      let next_h =
        match h with
        | East -> South
        | South -> West
        | West -> North
        | North -> East
        | White -> Green
        | Green -> Red
        | Red -> White
      in
      Honor next_h