(* lib/game.ml *)
type t = {
  deck : Deck.t;
  players : Player.t array;
  (* discard_pile : Tile.t list;  <-- 删除这一行 *)
  current_player_idx : int;
}

(* lib/game.ml 中的 create 函数替换为以下内容 *)

let create () =
  let deck = Deck.create () in
  let players = Array.init 4 (fun i -> Player.create (Printf.sprintf "Player%d" i)) in
  
  (* 1. 正常发牌给其他人（保持逻辑完整性） *)
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

  (* 2. [作弊] 强制覆盖 Player0 的手牌为 14 张完好的胡牌 *)
  (* 牌型：111 222 333 444 55 (万) *)
  (* let god_hand = [
    Tile.Numbered(Tile.Man, 1); Tile.Numbered(Tile.Man, 1); Tile.Numbered(Tile.Man, 1);
    Tile.Numbered(Tile.Man, 2); Tile.Numbered(Tile.Man, 2); Tile.Numbered(Tile.Man, 2);
    Tile.Numbered(Tile.Man, 3); Tile.Numbered(Tile.Man, 3); Tile.Numbered(Tile.Man, 3);
    Tile.Numbered(Tile.Man, 4); Tile.Numbered(Tile.Man, 4); Tile.Numbered(Tile.Man, 4);
    Tile.Numbered(Tile.Man, 5); Tile.Numbered(Tile.Man, 5); 
  ] in
  
  players.(0) <- Player.debug_set_hand players.(0) god_hand; *)

  {
    deck = final_deck;
    players = players;
    current_player_idx = 0;
  }
let current_player g =
  g.players.(g.current_player_idx)

let to_string g =
  let p_str = 
    Array.map Player.to_string g.players 
    |> Array.to_list 
    |> String.concat "\n\n" (* 增加换行让显示更清晰 *)
  in
  Printf.sprintf 
    "=== 游戏状态 ===\n当前玩家: %s\n剩余牌山: %d\n\n%s"
    (Player.name (current_player g))
    (Deck.remaining g.deck)
    p_str

let can_current_player_discard g =
  let p = current_player g in
  Player.has_full_hand p

let can_current_player_draw g =
  let p = current_player g in
  not (Player.has_full_hand p)
  
let draw_card g =
  if can_current_player_discard g then (* 如果已经满牌，不能摸 *)
    (g, None)
  else
    (* ... 原有的摸牌逻辑 ... *)
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

let next_turn g =
  { g with current_player_idx = (g.current_player_idx + 1) mod 4 }

let discard_card g tile =
  if can_current_player_draw g then (* 如果牌不够，不能打 *)
    (g, None)
  else
    (* ... 原有的打牌逻辑 ... *)
    let p = current_player g in
    match Player.discard_tile p tile with
    | None -> (g, None)
    | Some new_player ->
        let new_players = Array.copy g.players in
        new_players.(g.current_player_idx) <- new_player;
        let next_g = next_turn { g with players = new_players } in
        (next_g, Some tile)

let play_turn g =
  match draw_card g with
  | (g_after_draw, None) -> g_after_draw
  | (g_after_draw, Some _) ->
      let p = current_player g_after_draw in
      let hand_list = Player.hand p in
      match hand_list with
      | [] -> g_after_draw
      | h :: _ ->
          let (final_g, _) = discard_card g_after_draw h in
          final_g

let is_over g = Deck.remaining g.deck = 0

let winner g =
  let p = current_player g in
  if Player.can_tsumo p then Some p
  else None

let remaining_tiles g = Deck.remaining g.deck

let all_players g = Array.to_list g.players

(* lib/game.ml *)

(* ... 前面的结构体和 create, draw_card 保持不变 ... *)

(* 辅助：获取上一张打出的牌 *)
let last_discard g =
  let prev_idx = (g.current_player_idx - 1 + 4) mod 4 in
  let prev_p = g.players.(prev_idx) in
  match Player.discards prev_p with
  | [] -> None
  | h :: _ -> Some h

(* 吃 *)
let perform_chi g target t1 t2 =
  let p = current_player g in
  match Player.perform_chi p target t1 t2 with
  | None -> (g, false)
  | Some new_p ->
      let new_players = Array.copy g.players in
      new_players.(g.current_player_idx) <- new_p;
      ({ g with players = new_players }, true)




(* 修复：强制指定由玩家0 (人类) 发起杠 *)
let perform_kan g target =
  (* 1. 锁定玩家 0 *)
  let p = g.players.(0) in
  
  (* 2. 移除手牌，建立副露 *)
  match Player.perform_kan p target with
  | None -> 
      Printf.eprintf "[Error] 玩家0 试图杠 %s 失败\n" (Tile.to_string target);
      (g, false)
  | Some p_after_meld ->
      
      (* 3. 补杠/岭上牌：直接调用 Player.draw_tile *)
      (* 这会自动从 g.deck 摸一张牌加入 p_after_meld 的手牌中 *)
      match Player.draw_tile p_after_meld g.deck with
      | None -> (g, false) (* 牌堆空了，杠失败 *)
      | Some (p_final, final_deck) ->
          let new_players = Array.copy g.players in
          new_players.(0) <- p_final; (* 更新玩家0 *)
          
          (* 4. 抢夺回合：强制设为 0 *)
          ({
             deck = final_deck; 
             players = new_players;
             current_player_idx = 0 
           }, true)

(* --- 修复后的机器人逻辑 --- *)
let play_bot_step g =
  let p = current_player g in
  
  (* 修正：直接使用 Player.draw_tile，不需要先手动 Deck.draw *)
  match Player.draw_tile p g.deck with
  | None -> (g, false) (* 牌堆空了 *)
  | Some (p_with_card, deck_after_draw) ->
      
      (* 机器人逻辑：查看刚才摸到了什么牌 *)
      match Player.last_drawn p_with_card with
      | None -> (g, false) (* 理论上不应该发生 *)
      | Some tile_to_discard ->
          
          (* 机器人逻辑：立刻打出这张牌 (模切/Tsumogiri) *)
          match Player.discard_tile p_with_card tile_to_discard with
          | None -> (g, false)
          | Some p_after_discard ->
              let new_players = Array.copy g.players in
              new_players.(g.current_player_idx) <- p_after_discard;
              
              (* 轮转到下一家 *)
              let next_g = next_turn { 
                g with 
                deck = deck_after_draw; 
                players = new_players 
              } in
              (next_g, true)

(* auto_play_bots 保持不变 *)
let rec auto_play_bots g =
  if g.current_player_idx = 0 then g 
  else if Deck.remaining g.deck = 0 then g 
  else
    let (next_g, continue) = play_bot_step g in
    if continue then auto_play_bots next_g
    else next_g

let current_player_id g = g.current_player_idx

(* lib/game.ml *)
(* 注意：为了支持非当前玩家抢牌，我们需要传入 player_idx 或者直接指定是 Human 抢牌 *)
(* 但目前的 perform_pon 是从 g.current_player 取的，这不对。*)
(* 既然我们只做主玩家(Player0)，我们可以硬编码：*)

let perform_pon g target =
  (* 1. 永远是 Player 0 发起的碰 (因为只有人类能操作) *)
  let p0 = g.players.(0) in 
  
  match Player.perform_pon p0 target with
  | None -> (g, false)
  | Some new_p0 ->
      let new_players = Array.copy g.players in
      new_players.(0) <- new_p0; (* 更新 Player 0 *)
      
      (* 2. [关键] 还要处理被碰的那张牌 *)
      (* 按照规则，应该从弃牌堆拿走。目前我们是从 Player.hand 移除，
         但 UI 上只是显示副露。
         为了简单，我们暂时只更新 Player 0 的状态。*)

      (* 3. [关键] 强制将回合切回 Player 0 *)
      ({ g with 
         players = new_players; 
         current_player_idx = 0 (* 抢回回合！ *)
       }, true)