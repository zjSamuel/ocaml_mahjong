(* lib/game.ml *)
open Core

type t = { deck : Deck.t; players : Player.t array; current_player_idx : int }

(* ========================================== *)
(* 1. Initialization & Deal *)
(* ========================================== *)

let create () =
  let deck = Deck.create () in
  let players =
    Array.init 4 ~f:(fun i -> Player.create (Printf.sprintf "Player%d" i))
  in

  (* Initial Deal: Distribute 13 tiles to each player *)
  let rec deal_initial current_deck player_idx =
    if player_idx = 4 then (current_deck, players)
    else
      let rec draw_13 p d count =
        if count = 0 then (p, d)
        else
          match Player.draw_tile p d with
          | None -> (p, d)
          | Some (np, nd) -> draw_13 np nd (count - 1)
      in
      let p_full, d_final = draw_13 players.(player_idx) current_deck 13 in
      players.(player_idx) <- p_full;
      deal_initial d_final (player_idx + 1)
  in
  let deck_after_deal, _ = deal_initial deck 0 in

  (* ======================================================= *)
  (* DEBUG / CHEAT CONFIGURATION                             *)
  (* ======================================================= *)
  let enable_cheat = false in

  if enable_cheat then (
    let human_hand =
      [
        Tile.Numbered (Tile.Man, 1);
        Tile.Numbered (Tile.Man, 1);
        Tile.Numbered (Tile.Man, 1);
        Tile.Numbered (Tile.Man, 2);
        Tile.Numbered (Tile.Man, 2);
        Tile.Numbered (Tile.Man, 2);
        Tile.Numbered (Tile.Man, 3);
        Tile.Numbered (Tile.Man, 3);
        Tile.Numbered (Tile.Man, 3);
        Tile.Numbered (Tile.Man, 4);
        Tile.Numbered (Tile.Man, 4);
        Tile.Numbered (Tile.Sou, 9);
        Tile.Numbered (Tile.Man, 9);
      ]
    in
    players.(0) <- Player.debug_set_hand players.(0) human_hand;

    let ai_hand =
      [
        Tile.Numbered (Tile.Man, 1);
        Tile.Numbered (Tile.Man, 1);
        Tile.Numbered (Tile.Man, 1);
        Tile.Numbered (Tile.Man, 2);
        Tile.Numbered (Tile.Man, 2);
        Tile.Numbered (Tile.Man, 2);
        Tile.Numbered (Tile.Man, 3);
        Tile.Numbered (Tile.Man, 3);
        Tile.Numbered (Tile.Man, 3);
        Tile.Numbered (Tile.Man, 4);
        Tile.Numbered (Tile.Man, 4);
        Tile.Numbered (Tile.Sou, 9);
        Tile.Numbered (Tile.Man, 9);
      ]
    in
    players.(1) <- Player.debug_set_hand players.(1) ai_hand;

    let ai_hand =
      [
        Tile.Numbered (Tile.Man, 1);
        Tile.Numbered (Tile.Man, 1);
        Tile.Numbered (Tile.Man, 1);
        Tile.Numbered (Tile.Man, 2);
        Tile.Numbered (Tile.Man, 2);
        Tile.Numbered (Tile.Man, 2);
        Tile.Numbered (Tile.Man, 3);
        Tile.Numbered (Tile.Man, 3);
        Tile.Numbered (Tile.Man, 3);
        Tile.Numbered (Tile.Man, 4);
        Tile.Numbered (Tile.Man, 4);
        Tile.Numbered (Tile.Sou, 9);
        Tile.Numbered (Tile.Man, 9);
      ]
    in
    players.(2) <- Player.debug_set_hand players.(2) ai_hand;

        let ai_hand =
      [
        Tile.Numbered (Tile.Man, 1);
        Tile.Numbered (Tile.Man, 1);
        Tile.Numbered (Tile.Man, 1);
        Tile.Numbered (Tile.Man, 2);
        Tile.Numbered (Tile.Man, 2);
        Tile.Numbered (Tile.Man, 2);
        Tile.Numbered (Tile.Man, 3);
        Tile.Numbered (Tile.Man, 3);
        Tile.Numbered (Tile.Man, 3);
        Tile.Numbered (Tile.Man, 4);
        Tile.Numbered (Tile.Man, 4);
        Tile.Numbered (Tile.Sou, 9);
        Tile.Numbered (Tile.Man, 9);
      ]
    in
    players.(3) <- Player.debug_set_hand players.(3) ai_hand;

    (* let card_for_ai_win = Tile.Numbered (Tile.Man, 6) in *)
    (* let card_for_human = Tile.Honor Tile.West in *)

    (* let d1 = Deck.debug_force_next deck_after_deal card_for_ai_win in *)
    (* let final_deck = Deck.debug_force_next d1 card_for_human in *)

    { deck = deck_after_deal; players; current_player_idx = 0 })
  else { deck = deck_after_deal; players; current_player_idx = 0 }


let current_player g = g.players.(g.current_player_idx)
let current_player_id g = g.current_player_idx
let remaining_tiles g = Deck.remaining g.deck
let all_players g = Array.to_list g.players

let can_current_player_discard g =
  let p = current_player g in
  Player.has_full_hand p

let can_current_player_draw g =
  let p = current_player g in
  not (Player.has_full_hand p)

let is_over g = Deck.remaining g.deck = 0

let winner g =
  let p = current_player g in
  if Player.can_tsumo p then Some p else None

let last_discard g =
  let prev_idx = (g.current_player_idx - 1 + 4) % 4 in
  let prev_p = g.players.(prev_idx) in
  List.hd (Player.discards prev_p)

let add_tiles_to_counts counts tiles =
  List.iter tiles ~f:(fun t ->
      let id = Hand.tile_to_id t in
      counts.(id) <- counts.(id) + 1)

let get_visible_counts g viewer_idx =
  let counts = Array.create ~len:34 0 in

  Array.iter g.players ~f:(fun p ->
      add_tiles_to_counts counts (Player.discards p);
      List.iter (Player.melds p) ~f:(function
        | Hand.Chi (t1, t2, t3) -> add_tiles_to_counts counts [ t1; t2; t3 ]
        | Hand.Pon (t1, t2, t3) -> add_tiles_to_counts counts [ t1; t2; t3 ]
        | Hand.Kan (t1, t2, t3, t4) ->
            add_tiles_to_counts counts [ t1; t2; t3; t4 ]));

  let viewer = g.players.(viewer_idx) in
  add_tiles_to_counts counts (Player.hand viewer);

  counts

let get_dora_indicators g = Deck.get_dora_indicators g.deck

let draw_card g =
  if can_current_player_discard g then (g, None)
  else
    match Deck.draw g.deck with
    | None -> (g, None)
    | Some (tile, new_deck) -> (
        let p = current_player g in
        match Player.draw_tile p g.deck with
        | None ->
            (g, None)
        | Some (new_player, _) ->
            let new_players = Array.copy g.players in
            new_players.(g.current_player_idx) <- new_player;
            ({ g with deck = new_deck; players = new_players }, Some tile))

let next_turn g = { g with current_player_idx = (g.current_player_idx + 1) % 4 }

let discard_card g tile =
  if can_current_player_draw g then (g, None)
  else
    let p = current_player g in
    match Player.discard_tile p tile with
    | None -> (g, None)
    | Some new_player ->
        let new_players = Array.copy g.players in
        new_players.(g.current_player_idx) <- new_player;
        let next_g = next_turn { g with players = new_players } in
        (next_g, Some tile)

let perform_chi g target t1 t2 =
  let p = current_player g in
  match Player.perform_chi p target t1 t2 with
  | None -> (g, false)
  | Some new_p ->
      let new_players = Array.copy g.players in
      new_players.(g.current_player_idx) <- new_p;
      ({ g with players = new_players }, true)

let perform_pon g target =
  let p0 = g.players.(0) in
  match Player.perform_pon p0 target with
  | None -> (g, false)
  | Some new_p0 ->
      let new_players = Array.copy g.players in
      new_players.(0) <- new_p0;
      ({ g with players = new_players; current_player_idx = 0 }, true)

let perform_kan g target =
  let p = g.players.(0) in
  match Player.perform_kan p target with
  | None -> (g, false)
  | Some p_after_meld -> (
      let deck_flipped = Deck.add_dora_indicator g.deck in

      match Deck.draw_rinshan deck_flipped with
      | None -> (g, false)
      | Some (tile_drawn, final_deck) ->
          let p_final = Player.add_drawn_tile p_after_meld tile_drawn in
          let new_players = Array.copy g.players in
          new_players.(0) <- p_final;

          ( { deck = final_deck; players = new_players; current_player_idx = 0 },
            true ))

let set_bot_difficulty g idx diff =
  if idx < 0 || idx > 3 then g
  else
    let new_players = Array.copy g.players in
    new_players.(idx) <- Player.set_difficulty new_players.(idx) diff;
    { g with players = new_players }

let play_bot_step g =
  let p = current_player g in

  match Player.draw_tile p g.deck with
  | None -> (g, false)
  | Some (p_drawn, deck_after_draw) -> (
      if
        Player.can_tsumo p_drawn
      then (
        let nps = Array.copy g.players in
        nps.(g.current_player_idx) <- p_drawn;
        ({ g with deck = deck_after_draw; players = nps }, true))
      else
        let temp_players = Array.copy g.players in
        temp_players.(g.current_player_idx) <- p_drawn;
        let temp_game =
          { g with players = temp_players; deck = deck_after_draw }
        in

        let visible = get_visible_counts temp_game g.current_player_idx in
        let doras = Deck.get_dora_indicators g.deck in

        match Player.decide_discard p_drawn visible doras with
        | None -> (g, false)
        | Some tile_to_discard -> (
            match Player.discard_tile p_drawn tile_to_discard with
            | None -> (g, false)
            | Some p_after_discard ->
                let final_players = Array.copy g.players in
                final_players.(g.current_player_idx) <- p_after_discard;
                let next_g =
                  next_turn
                    { g with deck = deck_after_draw; players = final_players }
                in
                (next_g, true)))

let debug_set_player g idx p =
  let new_players = Array.copy g.players in
  new_players.(idx) <- p;
  { g with players = new_players }