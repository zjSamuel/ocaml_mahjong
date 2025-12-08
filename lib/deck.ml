(* deck.ml *)
(* lib/deck.ml *)

open Core

type t = {
  draw_pile : Tile.t list;       (** The live wall where players draw from *)
  dead_wall : Tile.t list;       (** The 14 tiles reserved as the Dead Wall *)
  dora_indicators_count : int;   (** How many dora indicators are currently revealed (1-5) *)
}

(** Generates a full list of 136 tiles (4 copies of each of the 34 types). *)
let create_full_list () =
  let suits = [ Tile.Man; Tile.Pin; Tile.Sou ] in
  let numbers = List.init 9 ~f:(fun i -> i + 1) in (* [1; ...; 9] *)
  
  (* Generate Numbered tiles *)
  let numbered =
    List.concat_map suits ~f:(fun s ->
      List.map numbers ~f:(fun n -> Tile.Numbered (s, n)))
  in
  
  (* Generate Honor tiles *)
  let honors =
    [
      Tile.East; Tile.South; Tile.West; Tile.North;
      Tile.Red; Tile.Green; Tile.White;
    ]
    |> List.map ~f:(fun h -> Tile.Honor h)
  in
  
  let unique_tiles = numbered @ honors in
  (* Create 4 copies of each tile *)
  List.concat_map unique_tiles ~f:(fun t -> [ t; t; t; t ])

(** Shuffles a list using a random sort approach. *)
let shuffle_list lst =
  let tagged = List.map lst ~f:(fun x -> (Random.bits (), x)) in
  (* Sort by the random tag *)
  let sorted = List.sort tagged ~compare:(fun (a, _) (b, _) -> Int.compare a b) in
  List.map sorted ~f:snd

let create () =
  Random.self_init ();

  let all = shuffle_list (create_full_list ()) in
  
  (* Split into Dead Wall (14 tiles) and Draw Pile (rest) *)
  let rec split n acc l =
    if n = 0 then (List.rev acc, l)
    else
      match l with
      | [] -> (List.rev acc, [])
      | h :: t -> split (n - 1) (h :: acc) t
  in
  let dead, draw = split 14 [] all in
  
  { draw_pile = draw; dead_wall = dead; dora_indicators_count = 1 }

let draw deck =
  match deck.draw_pile with
  | [] -> None
  | h :: t -> Some (h, { deck with draw_pile = t })

let remaining deck = List.length deck.draw_pile

let get_dora_indicators deck =
  let rec loop n acc =
    if n = 0 then List.rev acc
    else
      (* Indices in dead wall: 0, 2, 4, 6, 8 are indicators.
         1, 3, 5, 7, 9 are Ura-dora (hidden) indicators. *)
      let idx = (n - 1) * 2 in
      if idx < List.length deck.dead_wall then
        match List.nth deck.dead_wall idx with
        | Some tile -> loop (n - 1) (tile :: acc)
        | None -> acc (* Should not happen if logic is correct *)
      else acc
  in
  loop deck.dora_indicators_count []

let add_dora_indicator deck =
  if deck.dora_indicators_count < 5 then
    { deck with dora_indicators_count = deck.dora_indicators_count + 1 }
  else deck

let draw_rinshan deck =
  (* Simplified implementation: Draw from the draw pile for now.
     Real rules: Draw from dead wall, append from draw pile to dead wall to keep it at 14. *)
  draw deck

let debug_force_next deck tile =
  { deck with draw_pile = tile :: deck.draw_pile }