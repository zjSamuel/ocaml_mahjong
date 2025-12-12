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

let make_hand str_list = List.map str_list ~f:parse_tile_str
let empty_visible = Array.create ~len:34 0

let create_player_with_hand hand_strs =
  let p = Player.create "TestBot" in
  Player.debug_set_hand p (make_hand hand_strs)

let assert_string_contains str sub =
  let found = String.is_substring str ~substring:sub in
  if not found then
    assert_failure (Printf.sprintf "String %S does not contain %S" str sub)


let test_deck_basic _ =
  let d = Deck.create () in
  assert_equal 122 (Deck.remaining d) ~msg:"Initial deck size incorrect";
  let indicators = Deck.get_dora_indicators d in
  assert_equal 1 (List.length indicators) ~msg:"Start with 1 dora indicator"

let test_deck_rinshan_and_dora _ =
  let d = Deck.create () in
  let d_kan = Deck.add_dora_indicator d in
  assert_equal 2 (List.length (Deck.get_dora_indicators d_kan)) ~msg:"Should have 2 indicators after Kan";
  
  match Deck.draw_rinshan d_kan with
  | None -> assert_failure "Rinshan draw failed"
  | Some (_, d_after) ->
      assert_equal (Deck.remaining d - 1) (Deck.remaining d_after) ~msg:"Rinshan should consume a tile"


let test_win_hand _ =
  let hand = make_hand ["1m";"1m";"1p";"2p";"3p";"4s";"5s";"6s";"7s";"8s";"9s";"9m";"9m";"9m"] in
  assert_equal (-1) (Hand.calculate_shanten hand) ~msg:"Completed hand shanten should be -1";
  assert_bool "is_complete should be true" (Hand.is_complete hand)

let test_tenpai_hand _ =
  let hand = make_hand ["1m";"1m";"1p";"2p";"3p";"4s";"5s";"6s";"7s";"8s";"9s";"9m";"9m"] in
  assert_equal 0 (Hand.calculate_shanten hand) ~msg:"Tenpai hand shanten should be 0"

let test_yaku_tanyao _ =
  let hand = make_hand ["2m";"3m";"4m";"2p";"3p";"4p";"2s";"3s";"4s";"5s";"5s";"6s";"6s";"6s"] in
  match Hand.calculate_score hand [] [] Tile.East Tile.South true false with
  | None -> assert_failure "Should parse as valid hand"
  | Some result ->
      let has_tanyao = List.exists result.yaku_list ~f:(function Hand.Score.Tanyao -> true | _ -> false) in
      assert_bool "Should have Tanyao" has_tanyao

let test_yaku_yakuhai _ =
  let hand = make_hand ["5z";"5z";"5z";"1m";"2m";"3m";"9p";"9p";"1s";"2s";"3s";"4s";"4s";"4s"] in
  match Hand.calculate_score hand [] [] Tile.East Tile.South true false with
  | None -> assert_failure "Should parse as valid hand"
  | Some result ->
      let has_yakuhai = List.exists result.yaku_list ~f:(function Hand.Score.Yakuhai _ -> true | _ -> false) in
      assert_bool "Should have Yakuhai (White)" has_yakuhai


let test_find_chi_options _ =
  let p = create_player_with_hand ["2m"; "3m"; "5m"] in
  let target = Tile.Numbered(Tile.Man, 1) in
  let options = Player.find_chi_options p target in
  assert_equal 1 (List.length options);
  let t1, t2 = List.hd_exn options in
  assert_equal "2万" (Tile.to_string t1);
  assert_equal "3万" (Tile.to_string t2)

let test_perform_chi _ =
  let p = create_player_with_hand ["2m"; "3m"; "5m"] in
  let target = Tile.Numbered(Tile.Man, 4) in
  let t1 = Tile.Numbered(Tile.Man, 3) in
  let t2 = Tile.Numbered(Tile.Man, 5) in
  match Player.perform_chi p target t1 t2 with
  | None -> assert_failure "Perform chi failed"
  | Some new_p ->
      assert_equal 1 (List.length (Player.hand new_p));
      assert_equal 1 (List.length (Player.melds new_p))

let test_perform_pon _ =
  let p = create_player_with_hand ["5z"; "5z"; "6z"] in
  let target = Tile.Honor Tile.White in (* 5z *)
  match Player.perform_pon p target with
  | None -> assert_failure "Perform pon failed"
  | Some new_p ->
      assert_equal "发" (Tile.to_string (List.hd_exn (Player.hand new_p)));
      assert_equal 1 (List.length (Player.melds new_p))

let test_ai_difficulties _ =
  let p = create_player_with_hand ["1m";"2m";"3m";"5m";"6m";"7m";"1p";"2p";"3p";"9s";"9s";"1z";"2z";"3z"] in
  
  let p_easy = Player.set_difficulty p Player.Easy in
  let d_easy = Player.decide_discard p_easy empty_visible [] in
  assert_bool "Easy AI should discard something" (Option.is_some d_easy);

  let p_med = Player.set_difficulty p Player.Medium in
  let d_med = Player.decide_discard p_med empty_visible [] in
  assert_bool "Medium AI should discard something" (Option.is_some d_med);
  
  let p_hard = Player.set_difficulty p Player.Hard in
  let d_hard = Player.decide_discard p_hard empty_visible [] in
  assert_bool "Hard AI should discard something" (Option.is_some d_hard)

let test_to_string _ =
  let p = create_player_with_hand ["1m"; "2m"] in
  let s = Player.to_string p in
  assert_string_contains s "TestBot";
  assert_string_contains s "1万";
  assert_string_contains s "2万"


let test_game_create _ =
  let g = Game.create () in
  assert_equal 0 (Game.current_player_id g);
  assert_bool "Deck check" (Game.remaining_tiles g > 50);
  assert_equal 4 (List.length (Game.all_players g))

let test_draw_and_discard _ =
  let g = Game.create () in
  let g_drawn, tile_opt = Game.draw_card g in
  assert_bool "Draw valid" (Option.is_some tile_opt);
  
  let tile = Option.value_exn tile_opt in
  let g_disc, disc_opt = Game.discard_card g_drawn tile in
  assert_bool "Discard valid" (Option.is_some disc_opt);
  assert_equal 1 (Game.current_player_id g_disc) ~msg:"Turn should rotate"

let test_full_bot_step _ =
  let g = Game.create () in
  let g_p1 = Game.next_turn g in 
  
  let g_after, success = Game.play_bot_step g_p1 in
  assert_bool "Bot step success" success;
  assert_equal 2 (Game.current_player_id g_after) ~msg:"Bot should finish turn"

let test_interactions _ =
  let g = Game.create () in
  let p0 = Player.debug_set_hand (Game.current_player g) (make_hand ["2m";"3m";"1z";"1z"]) in
  let g = Game.debug_set_player g 0 p0 in
  
  let target = Tile.Numbered(Tile.Man, 1) in
  let t1 = Tile.Numbered(Tile.Man, 2) in
  let t2 = Tile.Numbered(Tile.Man, 3) in
  
  let g_chi, success = Game.perform_chi g target t1 t2 in
  assert_bool "Chi success" success;
  assert_equal 0 (Game.current_player_id g_chi) ~msg:"Turn remains for discard"

let suite =
  "MahjongTests" >::: [
    "test_deck_basic" >:: test_deck_basic;
    "test_deck_rinshan" >:: test_deck_rinshan_and_dora;
    "test_win_hand" >:: test_win_hand;
    "test_tenpai_hand" >:: test_tenpai_hand;
    "test_yaku_tanyao" >:: test_yaku_tanyao;
    "test_yaku_yakuhai" >:: test_yaku_yakuhai;
    "test_find_chi" >:: test_find_chi_options;
    "test_perform_chi" >:: test_perform_chi;
    "test_perform_pon" >:: test_perform_pon;
    "test_ai_difficulties" >:: test_ai_difficulties;
    "test_game_create" >:: test_game_create;
    "test_draw_discard" >:: test_draw_and_discard;
    "test_full_bot_step" >:: test_full_bot_step;
    "test_interactions" >:: test_interactions;
    "test_to_string" >:: test_to_string; 
  ]

let () = run_test_tt_main suite