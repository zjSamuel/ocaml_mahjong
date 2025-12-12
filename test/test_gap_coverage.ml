open Core
open OUnit2
open Mahjong


let parse_tile_str s =
  let val_char = s.[0] in
  let type_char = s.[1] in
  let n = Int.of_string (String.of_char val_char) in
  match type_char with
  | 'm' -> Tile.Numbered (Tile.Man, n)
  | 'p' -> Tile.Numbered (Tile.Pin, n)
  | 's' -> Tile.Numbered (Tile.Sou, n)
  | 'z' ->
      let h =
        match n with
        | 1 -> Tile.East | 2 -> Tile.South | 3 -> Tile.West | 4 -> Tile.North
        | 5 -> Tile.White | 6 -> Tile.Green | 7 -> Tile.Red
        | _ -> failwith "Invalid honor"
      in
      Tile.Honor h
  | _ -> failwith "Invalid suit"

let make_hand strs = List.map strs ~f:parse_tile_str
let empty_visible = Array.create ~len:34 0

let test_game_logic_coverage _ =
  let g = Game.create () in
  assert_bool "New game not over" (not (Game.is_over g));
  assert_equal None (Game.winner g);

  let rec drain_deck g =
    if Game.remaining_tiles g = 0 then g
    else match Game.draw_card g with
         | g', Some t -> 
             let g'', _ = Game.discard_card g' t in 
             drain_deck g''
         | g', None -> g'
  in
  let g_empty = drain_deck g in
  assert_bool "Empty deck means over" (Game.is_over g_empty);

  let win_hand = make_hand ["1m";"1m";"1m"; "2m";"2m";"2m"; "3m";"3m";"3m"; "4m";"4m";"4m"; "5m";"5m"] in
  let p0_win = Player.debug_set_hand (Game.current_player g) win_hand in
  let g_win = Game.debug_set_player g 0 p0_win in
  (match Game.winner g_win with
  | Some p -> assert_equal "Player0" (Player.name p)
  | None -> assert_failure "Should detect winner");

  assert_equal None (Game.last_discard g);
  let g_turn0, drawn = Game.draw_card g in
  let discard_tile = Option.value_exn drawn in
  let g_turn1, _ = Game.discard_card g_turn0 discard_tile in
  (match Game.last_discard g_turn1 with
  | Some t -> assert_bool "Last discard matches" (Tile.equal t discard_tile)
  | None -> assert_failure "Should have last discard");

  let g_vis = Game.create () in
  
  let p0_clean = Player.debug_set_hand (List.nth_exn (Game.all_players g_vis) 0) [] in
  let g_vis = Game.debug_set_player g_vis 0 p0_clean in

  let p1_orig = List.nth_exn (Game.all_players g_vis) 1 in
  let p1_setup = Player.debug_set_hand p1_orig (make_hand ["1m"; "2m"; "2m"; "3m"; "4m"]) in

  let p1_discarded = Player.discard_tile p1_setup (Tile.Numbered(Tile.Man, 1)) |> Option.value_exn in

  let p1_pon = Player.perform_pon p1_discarded (Tile.Numbered(Tile.Man, 2)) |> Option.value_exn in

  let g_vis = Game.debug_set_player g_vis 1 p1_pon in
  
  let counts = Game.get_visible_counts g_vis 0 in
  let id_1m = Hand.tile_to_id (Tile.Numbered(Tile.Man, 1)) in
  let id_2m = Hand.tile_to_id (Tile.Numbered(Tile.Man, 2)) in
  assert_equal 1 counts.(id_1m) ~msg:"1m count wrong";
  assert_equal 3 counts.(id_2m) ~msg:"2m count wrong";

  let g_pon_setup = Game.create () in
  let p0_ready = Player.debug_set_hand (Game.current_player g_pon_setup) (make_hand ["6z"; "6z"; "1m"]) in
  let g_pon_setup = Game.debug_set_player g_pon_setup 0 p0_ready in
  let g_poned, success = Game.perform_pon g_pon_setup (Tile.Honor Tile.Green) in
  assert_bool "Pon success" success;
  assert_equal 0 (Game.current_player_id g_poned);
  
  let g_kan_setup = Game.create () in
  let p0_kan_ready = Player.debug_set_hand (Game.current_player g_kan_setup) (make_hand ["7z"; "7z"; "7z"; "1m"]) in
  let g_kan_setup = Game.debug_set_player g_kan_setup 0 p0_kan_ready in
  let g_kaned, success = Game.perform_kan g_kan_setup (Tile.Honor Tile.Red) in
  assert_bool "Kan success" success;
  assert_equal 0 (Game.current_player_id g_kaned);
  let p0_after = Game.current_player g_kaned in
  assert_equal 2 (List.length (Player.hand p0_after));

  let g_diff = Game.create () in
  let g_diff = Game.set_bot_difficulty g_diff 1 Player.Hard in
  let p1_diff = List.nth_exn (Game.all_players g_diff) 1 in
  (match Player.difficulty p1_diff with
  | Player.Hard -> ()
  | _ -> assert_failure "Difficulty not set")


let test_tile_exhaustive _ =
  assert_equal 0 (Tile.compare_suit Tile.Man Tile.Man);
  assert_bool "Suit cmp" (Tile.compare_suit Tile.Man Tile.Pin < 0);
  assert_bool "Suit eq" (Tile.equal_suit Tile.Man Tile.Man);
  assert_equal 0 (Tile.compare_honor Tile.East Tile.East);
  assert_bool "Honor eq" (Tile.equal_honor Tile.East Tile.East);
  let t = Tile.Honor Tile.Red in
  let sexp = Tile.sexp_of_t t in
  assert_bool "Sexp roundtrip" (Tile.equal t (Tile.t_of_sexp sexp));

  let honors = [Tile.East; Tile.South; Tile.West; Tile.North; Tile.White; Tile.Green; Tile.Red] in
  List.iter honors ~f:(fun h ->
    let t = Tile.Honor h in
    let _ = Tile.to_string t in
    let _ = Tile.next_dora t in
    ()
  );
  let n1 = Tile.Numbered(Tile.Man, 1) in
  let n9 = Tile.Numbered(Tile.Man, 9) in
  let _ = Tile.next_dora n1 in 
  let _ = Tile.next_dora n9 in
  let _ = Tile.to_string n1 in
  ()

let test_player_predicates _ =
  let p = Player.create "PredBot" in
  let p_pon = Player.debug_set_hand p (make_hand ["1m"; "1m"; "5z"]) in
  assert_bool "Can pon" (Player.can_pon p_pon (parse_tile_str "1m"));
  assert_bool "No pon" (not (Player.can_pon p_pon (parse_tile_str "5z")));
  let p_kan = Player.debug_set_hand p (make_hand ["2m"; "2m"; "2m"]) in
  assert_bool "Can kan" (Player.can_kan p_kan (parse_tile_str "2m"));
  let p_ron = Player.debug_set_hand p (make_hand ["1m";"1m";"1m"; "2m";"2m";"2m"; "3m";"3m";"3m"; "4m";"4m";"4m"; "1p"]) in
  assert_bool "Can ron 1p" (Player.can_ron p_ron (parse_tile_str "1p"));
  assert_bool "No ron 2p" (not (Player.can_ron p_ron (parse_tile_str "2p")))

let test_player_string_melds _ =
  let p = Player.create "MeldBot" in
  let p = Player.debug_set_hand p (make_hand ["1z";"1z"]) in
  match Player.perform_pon p (parse_tile_str "1z") with
  | Some p -> assert_bool "Pon str" (String.is_substring (Player.to_string p) ~substring:"Pon")
  | None -> assert_failure "Pon fail"

let test_decide_discard_branches _ =
  let p = Player.create "Bot" in
  let win_hand = make_hand ["1m";"1m";"1m"; "2m";"2m";"2m"; "3m";"3m";"3m"; "4m";"4m";"4m"; "5m";"5m"] in
  let p = Player.debug_set_hand p win_hand in
  let p_med = Player.set_difficulty p Player.Medium in
  assert_bool "Med fallback" (Option.is_some (Player.decide_discard p_med empty_visible []));
  let p_hard = Player.set_difficulty p Player.Hard in
  assert_bool "Hard fallback" (Option.is_some (Player.decide_discard p_hard empty_visible []))

let test_ai_heuristics _ =
  let p = Player.create "AI" in
  let indicators = make_hand ["1m"] in
  let h_dora = make_hand ["2m";"2m";"2m";"5p";"5p";"5p";"1z";"1z";"1z";"2z";"2z";"3m";"4m";"8m"] in
  assert_bool "Dora" (List.length (Player.get_recommendations_enhanced (Player.debug_set_hand p h_dora) empty_visible indicators) > 0);
  let h_honitsu = make_hand ["1m";"2m";"3m"; "2m";"3m";"4m"; "5m";"6m";"7m"; "1z";"1z"; "8m";"9m";"9m"] in
  assert_bool "Honitsu" (List.length (Player.get_recommendations_enhanced (Player.debug_set_hand p h_honitsu) empty_visible []) > 0)

let test_player_failures _ =
  let p = Player.create "Fail" in
  let p = Player.debug_set_hand p [Tile.Numbered(Tile.Man, 1)] in
  assert_equal None (Player.discard_tile p (Tile.Numbered(Tile.Man, 2)));
  assert_equal None (Player.perform_pon p (Tile.Numbered(Tile.Man, 2)))

let test_player_basics _ =
  let p = Player.create "T" in
  let p = Player.set_difficulty p Player.Hard in
  (match Player.difficulty p with Player.Hard -> () | _ -> assert_failure "Diff")


let test_game_bot_branches _ =
  let g = Game.create () in
  let rec drain_all g =
    if Game.remaining_tiles g = 0 then g
    else match Game.draw_card g with g', Some t -> let g'',_ = Game.discard_card g' t in drain_all g'' | g', None -> g'
  in
  let g_empty = drain_all g in
  let _, success = Game.play_bot_step g_empty in
  assert_bool "Bot fails when deck empty" (not success);
  
  let g2 = Game.create () in
  let tenpai_hand = make_hand ["1m";"1m";"1m"; "2m";"2m";"2m"; "3m";"3m";"3m"; "4m";"4m";"4m"; "5m"] in
  let p1 = List.nth_exn (Game.all_players g2) 1 in
  let p1_ready = Player.debug_set_hand p1 tenpai_hand in
  let g2 = Game.debug_set_player g2 1 p1_ready in
  
  let _, drawn_opt = Game.draw_card g2 in
  let winning_tile = Option.value_exn drawn_opt in
  
  let hand_setup = 
    [Tile.Numbered(Tile.Man,1); Tile.Numbered(Tile.Man,1); Tile.Numbered(Tile.Man,1);
     Tile.Numbered(Tile.Man,2); Tile.Numbered(Tile.Man,2); Tile.Numbered(Tile.Man,2);
     Tile.Numbered(Tile.Man,3); Tile.Numbered(Tile.Man,3); Tile.Numbered(Tile.Man,3);
     Tile.Numbered(Tile.Man,4); Tile.Numbered(Tile.Man,4); Tile.Numbered(Tile.Man,4);
     winning_tile] 
  in
  let p1_cheat = Player.debug_set_hand p1 hand_setup in
  let g2_cheat = Game.debug_set_player g2 1 p1_cheat in
  let g2_turn = Game.next_turn g2_cheat in
  
  let g_after, success = Game.play_bot_step g2_turn in
  assert_bool "Bot successfully acted (Tsumo)" success;
  assert_equal 1 (Game.current_player_id g_after)


let suite =
  "GapCoverageTests" >::: [
    "test_tile_exhaustive" >:: test_tile_exhaustive;
    "test_game_logic_coverage" >:: test_game_logic_coverage;
    "test_game_bot_branches" >:: test_game_bot_branches;
    "test_player_predicates" >:: test_player_predicates;
    "test_player_string_melds" >:: test_player_string_melds;
    "test_decide_discard_branches" >:: test_decide_discard_branches;
    "test_ai_heuristics" >:: test_ai_heuristics;
    "test_player_failures" >:: test_player_failures;
    "test_player_basics" >:: test_player_basics;
  ]

let () = run_test_tt_main suite