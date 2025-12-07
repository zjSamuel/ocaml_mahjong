type t = {
  deck : Deck.t;
  players : Player.t array;
  (* discard_pile : Tile.t list *)
  current_player_idx : int;
}

let create () =
  let deck = Deck.create () in
  let players = Array.init 4 (fun i -> Player.create (Printf.sprintf "Player%d" i)) in
  
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
  let god_hand = [
    Tile.Numbered(Tile.Man, 1); Tile.Numbered(Tile.Man, 1); Tile.Numbered(Tile.Man, 2);
    Tile.Numbered(Tile.Man, 2); Tile.Numbered(Tile.Man, 3); Tile.Numbered(Tile.Man, 3);
    Tile.Numbered(Tile.Man, 4); Tile.Numbered(Tile.Man, 4); Tile.Numbered(Tile.Man, 5);
    Tile.Numbered(Tile.Man, 5); Tile.Numbered(Tile.Man, 6); Tile.Numbered(Tile.Man, 6);
    Tile.Numbered(Tile.Man, 7); 
  ] in
  
  players.(0) <- Player.debug_set_hand players.(0) god_hand;

  {
    deck = final_deck;
    players = players;
    current_player_idx = 0;
  }
let current_player g =
  g.players.(g.current_player_idx)

let can_current_player_discard g =
  let p = current_player g in
  Player.has_full_hand p

let can_current_player_draw g =
  let p = current_player g in
  not (Player.has_full_hand p)
  
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

let next_turn g =
  { g with current_player_idx = (g.current_player_idx + 1) mod 4 }

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

let is_over g = Deck.remaining g.deck = 0

let winner g =
  let p = current_player g in
  if Player.can_tsumo p then Some p
  else None

let remaining_tiles g = Deck.remaining g.deck

let all_players g = Array.to_list g.players

let last_discard g =
  let prev_idx = (g.current_player_idx - 1 + 4) mod 4 in
  let prev_p = g.players.(prev_idx) in
  match Player.discards prev_p with
  | [] -> None
  | h :: _ -> Some h

let perform_chi g target t1 t2 =
  let p = current_player g in
  match Player.perform_chi p target t1 t2 with
  | None -> (g, false)
  | Some new_p ->
      let new_players = Array.copy g.players in
      new_players.(g.current_player_idx) <- new_p;
      ({ g with players = new_players }, true)

let perform_kan g target =
  let p = g.players.(0) in
  match Player.perform_kan p target with
  | None -> 
      (* Printf.eprintf "[Error] 玩家0 试图杠 %s 失败\n" (Tile.to_string target); *)
      (g, false)
  | Some p_after_meld ->
      
      match Player.draw_tile p_after_meld g.deck with
      | None -> (g, false)
      | Some (p_final, final_deck) ->
          let new_players = Array.copy g.players in
          new_players.(0) <- p_final;
          ({
             deck = final_deck; 
             players = new_players;
             current_player_idx = 0 
           }, true)

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

let current_player_id g = g.current_player_idx

let perform_pon g target =
  let p0 = g.players.(0) in 
  
  match Player.perform_pon p0 target with
  | None -> (g, false)
  | Some new_p0 ->
      let new_players = Array.copy g.players in
      new_players.(0) <- new_p0;
      
      ({ g with 
         players = new_players; 
         current_player_idx = 0
       }, true)

let debug_set_player g idx p =
  let new_players = Array.copy g.players in
  new_players.(idx) <- p;
  { g with players = new_players }
(* lib/game.ml *)

(* 辅助：把 Tile 列表加入计数表 *)
let add_tiles_to_counts counts tiles =
  List.iter (fun t ->
    let id = Hand.tile_to_id t in
    counts.(id) <- counts.(id) + 1
  ) tiles

(* 获取全场可见牌的计数表 (对于视角玩家 viewer_idx 来说) *)
let get_visible_counts g viewer_idx =
  let counts = Array.make 34 0 in
  
  (* 1. 遍历所有玩家的 弃牌区 (Discards) 和 副露区 (Melds) *)
  Array.iter (fun p ->
    (* 加弃牌 *)
    add_tiles_to_counts counts (Player.discards p);
    
    (* 加副露 *)
    List.iter (function
      | Player.Chi(t1, t2, t3) -> add_tiles_to_counts counts [t1; t2; t3]
      | Player.Pon(t1, t2, t3) -> add_tiles_to_counts counts [t1; t2; t3]
      | Player.Kan(t1, t2, t3, t4) -> add_tiles_to_counts counts [t1; t2; t3; t4]
    ) (Player.melds p)
  ) g.players;

  (* 2. 加上视角玩家自己的手牌 (暗牌对别人不可见，对自己可见) *)
  let viewer = g.players.(viewer_idx) in
  add_tiles_to_counts counts (Player.hand viewer);
  
  counts