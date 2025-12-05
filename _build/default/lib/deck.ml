(* deck.ml *)
let () = Random.self_init ()

type t = {
  draw_pile : Tile.t list;
  _dead_wall : Tile.t list;
  _dora_index : int; 
  (* not applied *)
}

let create_full_list () =
  let suits = [Tile.Man; Tile.Pin; Tile.Sou] in
  let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let honors = [Tile.East; Tile.South; Tile.West; Tile.North; Tile.Red; Tile.Green; Tile.White] in
  let numbered = 
    List.concat_map (fun s -> List.map (fun n -> Tile.Numbered (s, n)) numbers) suits in
  let honored = List.map (fun h -> Tile.Honor h) honors in
  let unique = numbered @ honored in
  List.concat_map (fun t -> [t; t; t; t]) unique

let shuffle_list lst =
  let tagged = List.map (fun x -> (Random.bits (), x)) lst in
  let sorted = List.sort (fun (a, _) (b, _) -> compare a b) tagged in
  List.map snd sorted

let create_full () =
  let all = shuffle_list (create_full_list ()) in
  let rec split n acc l =
    if n = 0 then (List.rev acc, l)
    else match l with
      | [] -> (List.rev acc, [])
      | h :: t -> split (n - 1) (h :: acc) t
  in
  let (draw, dead) = split (List.length all - 14) [] all in
  { draw_pile = draw; _dead_wall = dead; _dora_index = 0 }

let create = create_full

let draw deck =
  match deck.draw_pile with
  | [] -> None
  | h :: t -> Some (h, { deck with draw_pile = t })

let remaining deck = List.length deck.draw_pile

let shuffle deck = 
  { deck with draw_pile = shuffle_list deck.draw_pile }

(* dont remove! not apply dora! *)
(* let dora_indicator deck =
  List.nth deck.dead_wall (deck.dora_index * 2) 

let next_dora_indicator deck =
  { deck with dora_index = deck.dora_index + 1 } *)