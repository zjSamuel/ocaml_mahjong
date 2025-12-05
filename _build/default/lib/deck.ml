(* deck.ml *)
let () = Random.self_init ()

type t = {
  draw_pile : Tile.t list;
  dead_wall : Tile.t list; (* 王牌区，包括宝牌指示牌 *)
  dora_index : int;        (* 当前翻开了第几张宝牌 *)
}

(* 生成所有牌 *)
let create_full_list () =
  let suits = [Tile.Man; Tile.Pin; Tile.Sou] in
  let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let honors = [Tile.East; Tile.South; Tile.West; Tile.North; Tile.Red; Tile.Green; Tile.White] in
  let numbered = 
    List.concat_map (fun s -> List.map (fun n -> Tile.Numbered (s, n)) numbers) suits in
  let honored = List.map (fun h -> Tile.Honor h) honors in
  let unique = numbered @ honored in
  List.concat_map (fun t -> [t; t; t; t]) unique

(* 洗牌算法 *)
let shuffle_list lst =
  let tagged = List.map (fun x -> (Random.bits (), x)) lst in
  let sorted = List.sort (fun (a, _) (b, _) -> compare a b) tagged in
  List.map snd sorted

let create_full () =
  let all = shuffle_list (create_full_list ()) in
  (* 简单的分割：最后 14 张作为王牌区 (Dead Wall) *)
  let rec split n acc l =
    if n = 0 then (List.rev acc, l)
    else match l with
      | [] -> (List.rev acc, [])
      | h :: t -> split (n - 1) (h :: acc) t
  in
  let (draw, dead) = split (List.length all - 14) [] all in
  { draw_pile = draw; dead_wall = dead; dora_index = 0 }

let create = create_full

let draw deck =
  match deck.draw_pile with
  | [] -> None
  | h :: t -> Some (h, { deck with draw_pile = t })

let rec draw_n deck n acc =
  if n <= 0 then (List.rev acc, deck)
  else
    match draw deck with
    | None -> (List.rev acc, deck)
    | Some (tile, next_deck) -> draw_n next_deck (n - 1) (tile :: acc)

let remaining deck = List.length deck.draw_pile

let shuffle deck = 
  (* 重新洗所有剩余的牌 (draw_pile) *)
  { deck with draw_pile = shuffle_list deck.draw_pile }

(* 宝牌逻辑: 假设 dead_wall 的第 3 张 (索引 2) 开始是宝牌指示牌 *)
(* 实际日麻规则：岭上牌4张 + 宝牌指示牌 *)
let dora_indicator deck =
  (* 简单实现：从 dead_wall 取对应索引 *)
  List.nth deck.dead_wall (deck.dora_index * 2) 
  (* *2 是因为通常还要预留里宝牌，这里简化处理 *)

let next_dora_indicator deck =
  { deck with dora_index = deck.dora_index + 1 }