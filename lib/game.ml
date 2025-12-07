(* lib/game.ml *)

type t = {
  deck : Deck.t;
  players : Player.t array;
  current_player_idx : int;
}

let create () =
  let deck = Deck.create () in
  let players = Array.init 4 (fun i -> Player.create (Printf.sprintf "Player%d" i)) in
  
  (* 1. 正常发牌 (先给所有人发13张随机牌) *)
  let rec deal_initial d idx =
    if idx = 4 then (d, players)
    else
      let rec draw_13 p d_inner count =
        if count = 0 then (p, d_inner)
        else match Player.draw_tile p d_inner with None -> (p, d_inner) | Some (np, nd) -> draw_13 np nd (count - 1)
      in
      let (p_full, d_final) = draw_13 players.(idx) d 13 in
      players.(idx) <- p_full;
      deal_initial d_final (idx + 1)
  in
  let (deck_after_deal, _) = deal_initial deck 0 in

  (* ======================================================= *)
  (* 🔧 调试配置区 (Debug Configuration)                    *)
  (* 修改这里来控制手牌和牌山                                *)
  (* ======================================================= *)
  
  (* 开关：设置为 true 启用作弊，false 则正常随机发牌 *)
  let enable_cheat = true in

  if enable_cheat then (
    (* A. 设定玩家 (Player 0) 的手牌 - 13张 *)
    (* 示例: 纯全带么九/混老头倾向，或者你想要的七对子 *)
    let human_hand = [
      Tile.Numbered(Tile.Man, 1); Tile.Numbered(Tile.Man, 9); 
      Tile.Numbered(Tile.Pin, 1); Tile.Numbered(Tile.Pin, 9);
      Tile.Numbered(Tile.Sou, 1); Tile.Numbered(Tile.Sou, 9);
      Tile.Honor(Tile.East); Tile.Honor(Tile.South); Tile.Honor(Tile.West); 
      Tile.Honor(Tile.North); Tile.Honor(Tile.White); Tile.Honor(Tile.Green); 
      Tile.Honor(Tile.Red); 
    ] in
    players.(0) <- Player.debug_set_hand players.(0) human_hand;

    (* B. 设定 AI (Player 1) 的手牌 - 13张听牌 *)
    (* 示例: 四暗刻单骑听牌型 (111 222 333 444 5m) *)
    let ai_hand = [
      Tile.Numbered(Tile.Man, 1); Tile.Numbered(Tile.Man, 1); Tile.Numbered(Tile.Man, 1);
      Tile.Numbered(Tile.Man, 2); Tile.Numbered(Tile.Man, 2); Tile.Numbered(Tile.Man, 2);
      Tile.Numbered(Tile.Man, 3); Tile.Numbered(Tile.Man, 3); Tile.Numbered(Tile.Man, 3);
      Tile.Numbered(Tile.Man, 4); Tile.Numbered(Tile.Man, 4); Tile.Numbered(Tile.Man, 4);
      Tile.Numbered(Tile.Man, 5); 
    ] in
    players.(1) <- Player.debug_set_hand players.(1) ai_hand;

    (* C. 操控牌山 (控制接下来的摸牌) *)
    (* 注意顺序：列表的头部是"最后"放上去的牌，也就是"最先"被摸到的牌 *)
    (* 游戏开始流程: P0摸牌 -> P0切牌 -> P1摸牌(AI) *)
    (* 所以我们需要: 
       1. P1 赢的牌 (AI Will Draw) -> 放在第2张
       2. P0 随便摸的牌 (Human Will Draw) -> 放在第1张 (顶端)
    *)
    
    let card_for_ai_win = Tile.Numbered(Tile.Man, 5) in  (* AI 需要 5万 自摸 *)
    let card_for_human  = Tile.Honor(Tile.West) in       (* 给人类一张废牌 *)

    (* 压栈操作: 先压 AI 的牌，再压人类的牌，这样人类先摸 *)
    let d1 = Deck.debug_force_next deck_after_deal card_for_ai_win in
    let final_deck = Deck.debug_force_next d1 card_for_human in

    { deck = final_deck; players = players; current_player_idx = 0; }
  ) else (
    { deck = deck_after_deal; players = players; current_player_idx = 0; }
  )
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
  (* 1. 机器人摸牌 *)
  match Player.draw_tile p g.deck with
  | None -> (g, false)
  | Some (p_drawn, deck_after_draw) ->
      (* 2. [关键] 判断是否自摸 *)
      if Player.can_tsumo p_drawn then
        let nps = Array.copy g.players in
        nps.(g.current_player_idx) <- p_drawn;
        (* 自摸了！不切牌，不流转回合，直接返回状态，让 controller (main.ml) 处理胜利 *)
        ({ g with deck = deck_after_draw; players = nps }, true)
      else
        (* 3. 没胡，执行切牌 (这里简单切摸到的牌) *)
        match Player.last_drawn p_drawn with
        | None -> (g, false)
        | Some tile_to_discard ->
            match Player.discard_tile p_drawn tile_to_discard with
            | None -> (g, false)
            | Some p_after_discard ->
                let nps = Array.copy g.players in
                nps.(g.current_player_idx) <- p_after_discard;
                let next_g = next_turn { g with deck = deck_after_draw; players = nps } in
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