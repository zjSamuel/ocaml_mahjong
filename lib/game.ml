(* lib/game.ml *)

type t = {
  deck : Deck.t;
  players : Player.t array;
  current_player_idx : int;
}

let create () =
  let deck = Deck.create () in
  let players = Array.init 4 (fun i -> Player.create (Printf.sprintf "Player%d" i)) in
  
  (* 正常发牌流程：给每人发13张 *)
  let rec deal_initial d idx =
    if idx = 4 then (d, players)
    else
      let rec draw_13 p d_inner count =
        if count = 0 then (p, d_inner)
        else
          match Player.draw_tile p d_inner with
          | None -> (p, d_inner)
          | Some (p_next, d_next_inner) -> draw_13 p_next d_next_inner (count - 1)
      in
      let (p_full, d_final) = draw_13 players.(idx) d 13 in
      players.(idx) <- p_full;
      deal_initial d_final (idx + 1)
  in
  let (final_deck, _) = deal_initial deck 0 in

  (* --- 🔧 God Hand Cheat Code (已加回) --- *)
  (* 牌型：11 22 33 44 55 66 7 (万子清一色七对子听牌型) *)
  let god_hand = [
    Tile.Numbered(Tile.Man, 1); Tile.Numbered(Tile.Man, 1); Tile.Numbered(Tile.Man, 2);
    Tile.Numbered(Tile.Man, 2); Tile.Numbered(Tile.Man, 3); Tile.Numbered(Tile.Man, 3);
    Tile.Numbered(Tile.Man, 4); Tile.Numbered(Tile.Man, 4); Tile.Numbered(Tile.Man, 5);
    Tile.Numbered(Tile.Man, 5); Tile.Numbered(Tile.Man, 6); Tile.Numbered(Tile.Man, 6);
    Tile.Numbered(Tile.Man, 7); Tile.Numbered(Tile.Man, 7);
  ] in
  
  (* 强制覆盖玩家0的手牌 *)
  players.(0) <- Player.debug_set_hand players.(0) god_hand;
  (* ------------------------------------- *)

  {
    deck = final_deck;
    players = players;
    current_player_idx = 0;
  }
(* 基础访问器 *)
let current_player g = g.players.(g.current_player_idx)
let current_player_id g = g.current_player_idx

(* 状态判定 *)
let can_current_player_discard g =
  let p = current_player g in
  Player.has_full_hand p

let can_current_player_draw g =
  let p = current_player g in
  not (Player.has_full_hand p)
  
(* 摸牌动作 *)
let draw_card g =
  if can_current_player_discard g then
    (g, None)
  else
    match Deck.draw g.deck with
    | None -> (g, None)
    | Some (tile, _) ->
        let p = current_player g in
        match Player.draw_tile p g.deck with
        | None -> (g, None)
        | Some (new_player, new_deck) ->
            let new_players = Array.copy g.players in
            new_players.(g.current_player_idx) <- new_player;
            ({ g with deck = new_deck; players = new_players }, Some tile)

(* 回合流转 *)
let next_turn g =
  { g with current_player_idx = (g.current_player_idx + 1) mod 4 }

(* 切牌动作 *)
let discard_card g tile =
  if can_current_player_draw g then 
    (g, None)
  else
    let p = current_player g in
    match Player.discard_tile p tile with
    | None -> (g, None)
    | Some new_player ->
        let new_players = Array.copy g.players in
        new_players.(g.current_player_idx) <- new_player;
        let next_g = next_turn { g with players = new_players } in
        (next_g, Some tile)

(* 游戏结束判定 *)
let is_over g = Deck.remaining g.deck = 0

let winner g =
  let p = current_player g in
  if Player.can_tsumo p then Some p
  else None

let remaining_tiles g = Deck.remaining g.deck

let all_players g = Array.to_list g.players

(* 获取上家弃牌（用于吃碰杠判定） *)
let last_discard g =
  let prev_idx = (g.current_player_idx - 1 + 4) mod 4 in
  let prev_p = g.players.(prev_idx) in
  match Player.discards prev_p with
  | [] -> None
  | h :: _ -> Some h

(* 吃 (Chi) *)
let perform_chi g target t1 t2 =
  let p = current_player g in
  match Player.perform_chi p target t1 t2 with
  | None -> (g, false)
  | Some new_p ->
      let new_players = Array.copy g.players in
      new_players.(g.current_player_idx) <- new_p;
      ({ g with players = new_players }, true)

(* 碰 (Pon) *)
let perform_pon g target =
  let p0 = g.players.(0) in 
  match Player.perform_pon p0 target with
  | None -> (g, false)
  | Some new_p0 ->
      let new_players = Array.copy g.players in
      new_players.(0) <- new_p0;
      (* 碰完后轮到自己切牌 *)
      ({ g with 
         players = new_players; 
         current_player_idx = 0
       }, true)

(* [修改] 杠 (Kan) - 包含开新宝牌和岭上摸牌逻辑 *)
let perform_kan g target =
  let p = g.players.(0) in
  match Player.perform_kan p target with
  | None -> (g, false)
  | Some p_after_meld ->
      (* 1. 翻开新的宝牌指示牌 *)
      let deck_flipped = Deck.add_dora_indicator g.deck in
      
      (* 2. 从岭上摸一张牌 *)
      match Deck.draw_rinshan deck_flipped with
      | None -> (g, false)
      | Some (tile_drawn, final_deck) ->
          (* 3. 将摸到的牌加入玩家手牌 *)
          let p_final = Player.add_drawn_tile p_after_meld tile_drawn in
          
          let new_players = Array.copy g.players in
          new_players.(0) <- p_final;
          
          ({
             deck = final_deck; 
             players = new_players;
             current_player_idx = 0 
           }, true)

(* 机器人简单逻辑 *)
let play_bot_step g =
  let p = current_player g in
  match Player.draw_tile p g.deck with
  | None -> (g, false)
  | Some (p_with_card, deck_after_draw) ->
      match Player.last_drawn p_with_card with
      | None -> (g, false)
      | Some tile_to_discard ->
          match Player.discard_tile p_with_card tile_to_discard with
          | None -> (g, false)
          | Some p_after_discard ->
              let new_players = Array.copy g.players in
              new_players.(g.current_player_idx) <- p_after_discard;
              let next_g = next_turn { 
                g with 
                deck = deck_after_draw; 
                players = new_players 
              } in
              (next_g, true)

let debug_set_player g idx p =
  let new_players = Array.copy g.players in
  new_players.(idx) <- p;
  { g with players = new_players }

(* [新增] 统计场上可见牌逻辑 *)
let add_tiles_to_counts counts tiles =
  List.iter (fun t ->
    let id = Hand.tile_to_id t in
    counts.(id) <- counts.(id) + 1
  ) tiles

let get_visible_counts g viewer_idx =
  let counts = Array.make 34 0 in
  
  (* 遍历所有玩家的弃牌区和副露区 *)
  Array.iter (fun p ->
    add_tiles_to_counts counts (Player.discards p);
    
    (* [重要修改] 这里使用了 Hand.Chi/Pon/Kan *)
    List.iter (function
      | Hand.Chi(t1, t2, t3) -> add_tiles_to_counts counts [t1; t2; t3]
      | Hand.Pon(t1, t2, t3) -> add_tiles_to_counts counts [t1; t2; t3]
      | Hand.Kan(t1, t2, t3, t4) -> add_tiles_to_counts counts [t1; t2; t3; t4]
    ) (Player.melds p)
  ) g.players;

  (* 加上观察者自己的手牌 *)
  let viewer = g.players.(viewer_idx) in
  add_tiles_to_counts counts (Player.hand viewer);
  
  counts

(* 获取宝牌指示牌 *)
let get_dora_indicators g =
  Deck.get_dora_indicators g.deck