(* deck.ml *)
let () = Random.self_init ()

type t = {
  draw_pile : Tile.t list;
  dead_wall : Tile.t list; (* 王牌区 *)
  dora_indicators_count : int; (* 当前开了几张宝牌指示牌 *)
}

let create_full_list () =
  let suits = [ Tile.Man; Tile.Pin; Tile.Sou ] in
  let numbers = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  let honors =
    [
      Tile.East;
      Tile.South;
      Tile.West;
      Tile.North;
      Tile.Red;
      Tile.Green;
      Tile.White;
    ]
  in
  let numbered =
    List.concat_map
      (fun s -> List.map (fun n -> Tile.Numbered (s, n)) numbers)
      suits
  in
  let honored = List.map (fun h -> Tile.Honor h) honors in
  let unique = numbered @ honored in
  List.concat_map (fun t -> [ t; t; t; t ]) unique

let shuffle_list lst =
  let tagged = List.map (fun x -> (Random.bits (), x)) lst in
  let sorted = List.sort (fun (a, _) (b, _) -> compare a b) tagged in
  List.map snd sorted

let create () =
  let all = shuffle_list (create_full_list ()) in
  let rec split n acc l =
    if n = 0 then (List.rev acc, l)
    else
      match l with
      | [] -> (List.rev acc, [])
      | h :: t -> split (n - 1) (h :: acc) t
  in
  (* 留 14 张作为王牌 (Dead Wall) *)
  let dead, draw = split 14 [] all in
  { draw_pile = draw; dead_wall = dead; dora_indicators_count = 1 }

let draw deck =
  match deck.draw_pile with
  | [] -> None
  | h :: t -> Some (h, { deck with draw_pile = t })

let remaining deck = List.length deck.draw_pile

(* 获取当前的宝牌指示牌列表 *)
let get_dora_indicators deck =
  let rec loop n acc =
    if n = 0 then List.rev acc
    else
      (* 王牌区的第 0, 2, 4, 6, 8 张是表宝牌指示牌 *)
      let idx = (n - 1) * 2 in
      if idx < List.length deck.dead_wall then
        loop (n - 1) (List.nth deck.dead_wall idx :: acc)
      else acc
  in
  loop deck.dora_indicators_count []

(* 开杠：增加一张宝牌指示牌 *)
let add_dora_indicator deck =
  if deck.dora_indicators_count < 5 then
    { deck with dora_indicators_count = deck.dora_indicators_count + 1 }
  else deck

(* 岭上开花：从王牌区摸牌 *)
let draw_rinshan deck =
  (* 简单实现：从王牌末尾拿一张，同时把摸牌堆顶的一张补进王牌以保持14张王牌 *)
  (* 注意：标准规则比较复杂，这里简化为从 draw_pile 拿 *)
  draw deck

(* [新增] 调试用：强制将一张牌放在牌山顶端 *)
let debug_force_next deck tile =
  { deck with draw_pile = tile :: deck.draw_pile }
