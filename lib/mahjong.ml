(* lib/mahjong.ml *)
let () = Random.self_init ()

module Tile = struct
  type suit =
    | Man
    | Pin
    | Sou

  type honor =
    | East  | South | West  | North
    | Red   | Green | White

  type t =
    | Numbered of suit * int
    | Honor of honor

  let compare t1 t2 =
    match (t1, t2) with
    | Numbered _, Honor _ -> -1
    | Honor _, Numbered _ -> 1
    | Numbered (s1, n1), Numbered (s2, n2) ->
      let suit_cmp = compare s1 s2 in
      if suit_cmp <> 0 then suit_cmp else compare n1 n2
    | Honor h1, Honor h2 -> compare h1 h2

  let to_string = function
    | Numbered (suit, n) ->
      let suit_str = match suit with | Man -> "万" | Pin -> "筒" | Sou -> "索" in
      (string_of_int n) ^ suit_str
    | Honor honor ->
      match honor with
      | East -> "东" | South -> "南" | West -> "西" | North -> "北"
      | Red -> "中" | Green -> "发" | White -> "白"
end

module Deck = struct
  type t = Tile.t list

  let create_full () : t =
    let suits = [Tile.Man; Tile.Pin; Tile.Sou] in
    let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    let honors = [
      Tile.East; Tile.South; Tile.West; Tile.North;
      Tile.Red; Tile.Green; Tile.White
    ] in
    let numbered_tiles =
      List.concat_map (fun s ->
        List.map (fun n -> Tile.Numbered (s, n)) numbers
      ) suits
    in
    let honor_tiles = List.map (fun h -> Tile.Honor h) honors in
    let all_unique_tiles = numbered_tiles @ honor_tiles in
    List.concat_map (fun tile -> [tile; tile; tile; tile]) all_unique_tiles

  let shuffle (deck: t) : t =
    let paired = List.map (fun card -> (Random.bits (), card)) deck in
    let sorted = List.sort (fun (a, _) (b, _) -> compare a b) paired in
    List.map (fun (_, card) -> card) sorted

  let create () : t =
    create_full () |> shuffle

  let draw (deck: t) : (Tile.t option * t) =
    match deck with
    | [] -> (None, [])
    | head :: tail -> (Some head, tail)

  let rec draw_n (deck: t) (n: int) (drawn: Tile.t list) : (Tile.t list * t) =
    if n <= 0 then
      (drawn, deck)
    else
      match draw deck with
      | (None, remaining_deck) -> (drawn, remaining_deck)
      | (Some tile, remaining_deck) ->
        draw_n remaining_deck (n - 1) (tile :: drawn)
end

module Player = struct
  type hand = Tile.t list
  type t = {
    id: int;
    hand: hand;
  }

  let create (id: int) : t =
    { id; hand = [] }

  let sort_hand (h: hand) : hand =
    List.sort Tile.compare h

  let rec remove_first (tile: Tile.t) (h: hand) : hand =
    match h with
    | [] -> []
    | hd :: tl ->
      if Tile.compare hd tile = 0 then tl
      else hd :: (remove_first tile tl)

  let add_to_hand (tile: Tile.t) (p: t) : t =
    let new_hand = sort_hand (tile :: p.hand) in
    { p with hand = new_hand }

  let discard_tile (tile: Tile.t) (p: t) : t option =
    if List.mem tile p.hand then
      let new_hand = remove_first tile p.hand in
      Some { p with hand = new_hand }
    else
      None

  let hand_to_string (h: hand) : string =
    h
    |> List.map Tile.to_string
    |> String.concat " "

  let to_string (p: t) : string =
    Printf.sprintf "玩家 %d 手牌: [ %s ]" p.id (hand_to_string p.hand)
end

module Game = struct
  type t = {
    deck: Deck.t;
    players: Player.t array;
    discard_pile: Tile.t list;
    current_player_idx: int;
  }

  let create () : t =
    let deck = Deck.create () in
    let players = Array.init 4 Player.create in
    let (p0_hand_list, deck1) = Deck.draw_n deck 13 [] in
    let (p1_hand_list, deck2) = Deck.draw_n deck1 13 [] in
    let (p2_hand_list, deck3) = Deck.draw_n deck2 13 [] in
    let (p3_hand_list, final_deck) = Deck.draw_n deck3 13 [] in
    players.(0) <- { players.(0) with hand = Player.sort_hand p0_hand_list };
    players.(1) <- { players.(1) with hand = Player.sort_hand p1_hand_list };
    players.(2) <- { players.(2) with hand = Player.sort_hand p2_hand_list };
    players.(3) <- { players.(3) with hand = Player.sort_hand p3_hand_list };
    {
      deck = final_deck;
      players = players;
      discard_pile = [];
      current_player_idx = 0;
    }

  let to_string (g: t) : string =
    let player_strings =
      g.players
      |> Array.to_list
      |> List.map Player.to_string
      |> String.concat "\n"
    in
    let discard_string =
      g.discard_pile
      |> List.rev
      |> List.map Tile.to_string
      |> String.concat ", "
    in
    Printf.sprintf
      "--- 游戏状态 ---\n%s\n牌堆剩余: %d 张\n弃牌堆: [ %s ]\n当前玩家: %d\n"
      player_strings
      (List.length g.deck)
      discard_string
      g.current_player_idx

(* 在 lib/mahjong.ml 的 module Game = struct ... end 内部 ... *)

(* ... 省略 create 和 to_string ... *)

(* 旧的 play_turn 函数应被删除或注释掉。
   我们用下面两个新函数替换它：
*)

(* 步骤 1：玩家摸牌 *)
(* 返回 (更新后的游戏状态, 摸到的牌) *)
let draw_card (g: t) : (t * Tile.t option) =
  let player_idx = g.current_player_idx in
  let player = g.players.(player_idx) in
  
  (* 1. 从牌堆摸牌 *)
  let (drawn_tile_opt, new_deck) = Deck.draw g.deck in

  match drawn_tile_opt with
  | None -> (g, None) (* 牌堆空了，无法摸牌 *)
  | Some drawn_tile ->
    (* 2. 牌加入手牌 (玩家现在有 14 张) *)
    let player_with_14 = Player.add_to_hand drawn_tile player in
    
    (* 3. 更新游戏状态中的玩家数组 *)
    let new_players = Array.copy g.players in
    new_players.(player_idx) <- player_with_14;
    
    (* 4. 返回新状态和摸到的牌 *)
    ({ g with deck = new_deck; players = new_players }, Some drawn_tile)

(* 步骤 2：玩家根据输入打牌 *)
(* 假设当前玩家已有 14 张牌，此函数将其减少到 13 张并轮到下一家 *)
(* 返回 (更新后的游戏状态, 实际打出的牌) *)
let discard_card (g: t) (tile_to_discard: Tile.t) : (t * Tile.t option) =
  let player_idx = g.current_player_idx in
  let player = g.players.(player_idx) in (* 该玩家应有 14 张牌 *)

  (* 1. 玩家打牌 *)
  match Player.discard_tile tile_to_discard player with
  | None ->
    (* 这种情况不应该发生，除非逻辑错误 *)
    Printf.printf "错误：玩家 %d 试图打出一张他没有的牌 %s\n"
      player.id (Tile.to_string tile_to_discard);
    (g, None) (* 回合失败，状态不推进 *)
  | Some player_after_discard ->
    (* 2. 更新玩家数组 *)
    let new_players = Array.copy g.players in
    new_players.(player_idx) <- player_after_discard;

    (* 3. 更新游戏状态：弃牌堆 和 下一个玩家 *)
    let new_game_state = {
      g with
      players = new_players;
      discard_pile = tile_to_discard :: g.discard_pile;
      current_player_idx = (player_idx + 1) mod 4; (* 轮到下一个玩家 *)
    } in
    (new_game_state, Some tile_to_discard)

end