open OUnit2
open Mahjong

let parse_tile_str s =
  let _len = String.length s in
  let val_char = s.[0] in
  let type_char = s.[1] in
  let n = int_of_string (String.make 1 val_char) in
  match type_char with
  | 'm' -> Tile.Numbered(Tile.Man, n)
  | 'p' -> Tile.Numbered(Tile.Pin, n)
  | 's' -> Tile.Numbered(Tile.Sou, n)
  | 'z' -> 
      let h = match n with
        | 1 -> Tile.East | 2 -> Tile.South | 3 -> Tile.West | 4 -> Tile.North
        | 5 -> Tile.White | 6 -> Tile.Green | 7 -> Tile.Red
        | _ -> failwith "Invalid honor"
      in Tile.Honor h
  | _ -> failwith "Invalid suit"

let make_hand str_list =
  List.map parse_tile_str str_list

let test_win_hand _ =
  let hand = make_hand ["1m";"1m"; "1p";"2p";"3p"; "4s";"5s";"6s"; "7s";"8s";"9s"; "9m";"9m"; "9m"] in
  assert_equal (-1) (Hand.calculate_shanten hand) ~msg:"Standard win should correspond to -1 shanten";
  assert_bool "Should be complete" (Hand.is_complete hand)

let test_efficiency _ =
  let hand = make_hand ["1m";"1m";"1m"; "2p";"2p";"2p"; "3s";"3s";"3s"; "4z";"4z";"4z"; "5z"; "6z"] in
  let recommendations = Hand.calculate_efficiency hand in
  let (top_tile, count) = List.hd recommendations in
  assert_bool "Should recommend discard" (count > 0);
  match top_tile with
  | Tile.Honor _ -> ()
  | _ -> assert_failure "Should recommend discarding isolated honor tile"

  let test_game_flow _ =
  let g = Game.create () in
  assert_equal 0 (Game.current_player_id g) ~msg:"Initial player should be 0";
  assert_bool "Deck should not be empty" (Game.remaining_tiles g > 0);
  let p = Game.current_player g in
  assert_equal 13 (Player.tile_count p) ~msg:"Player should start with 13 tiles"

let create_player_with_hand hand_strs =
  let p = Player.create "TestBot" in
  Player.debug_set_hand p (make_hand hand_strs)

let test_find_chi_options _ =
  let p = create_player_with_hand ["2m"; "3m"; "5m"] in
  let target = Tile.Numbered(Tile.Man, 1) in
  let options = Player.find_chi_options p target in
  
  assert_equal 1 (List.length options) ~msg:"Should find 1 chi option";
  let (t1, t2) = List.hd options in
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
      assert_equal "2万" (Tile.to_string (List.hd (Player.hand new_p)));
      assert_equal 1 (List.length (Player.melds new_p));
      match List.hd (Player.melds new_p) with
      | Player.Chi _ -> ()
      | _ -> assert_failure "Meld type should be Chi"

let test_pon _ =
  let p = create_player_with_hand ["5z"; "5z"; "6z"] in
  let target = Tile.Honor(Tile.White) in
  
  assert_bool "Should be able to pon" (Player.can_pon p target);
  
  match Player.perform_pon p target with
  | None -> assert_failure "Perform pon failed"
  | Some new_p ->
      assert_equal 1 (List.length (Player.hand new_p));
      assert_equal "发" (Tile.to_string (List.hd (Player.hand new_p)));
      match List.hd (Player.melds new_p) with
      | Player.Pon _ -> ()
      | _ -> assert_failure "Meld type should be Pon"

let test_kan _ =
  let p = create_player_with_hand ["7z"; "7z"; "7z"] in
  let target = Tile.Honor(Tile.Red) in
  
  assert_bool "Should be able to kan" (Player.can_kan p target);
  
  match Player.perform_kan p target with
  | None -> assert_failure "Perform kan failed"
  | Some new_p ->
      assert_equal 0 (List.length (Player.hand new_p));
      match List.hd (Player.melds new_p) with
      | Player.Kan _ -> ()
      | _ -> assert_failure "Meld type should be Kan"

let test_draw_discard _ =
  let p = Player.create "Tester" in
  let deck = Deck.create () in
  
  match Player.draw_tile p deck with
  | None -> assert_failure "Draw failed"
  | Some (p_drawn, _) ->
      assert_equal 1 (List.length (Player.hand p_drawn));
      
      let tile_to_discard = List.hd (Player.hand p_drawn) in
      match Player.discard_tile p_drawn tile_to_discard with
      | None -> assert_failure "Discard failed"
      | Some p_final ->
          assert_equal 0 (List.length (Player.hand p_final));
          assert_equal 1 (List.length (Player.discards p_final))

let assert_string_contains str sub =
  let len = String.length str in
  let sub_len = String.length sub in
  let found = ref false in
  for i = 0 to len - sub_len do
    if String.sub str i sub_len = sub then found := true
  done;
  if not !found then 
    assert_failure (Printf.sprintf "String %S does not contain %S" str sub)

let test_to_string _ =
  let p = create_player_with_hand ["1m"; "2m"; "3m"] in
  let s = Player.to_string p in
  assert_string_contains s "TestBot";
  assert_string_contains s "1万"; 
  assert_string_contains s "2万"

let test_has_full_hand _ =
  let p_13 = create_player_with_hand 
    ["1m"; "1m"; "1m"; "2m"; "2m"; "2m"; "3m"; "3m"; "3m"; "4m"; "4m"; "4m"; "5m"] in
  assert_equal 13 (Player.tile_count p_13);
  assert_bool "Should not be full hand" (not (Player.has_full_hand p_13));

  let deck = Deck.create () in
  match Player.draw_tile p_13 deck with
  | Some (p_14, _) ->
      assert_equal 14 (Player.tile_count p_14);
      assert_bool "Should be full hand" (Player.has_full_hand p_14)
  | None -> assert_failure "Draw failed"

let test_can_ron _ =
  let p = create_player_with_hand 
    ["1m";"1m";"1m"; "2m";"2m";"2m"; "3m";"3m"; "3m"; "4m";"4m"; "4m"; "5m"] in
  
  let win_tile = Tile.Numbered(Tile.Man, 5) in
  let wrong_tile = Tile.Numbered(Tile.Man, 7) in
  
  assert_bool "Should can ron on 5m" (Player.can_ron p win_tile);
  assert_bool "Should not ron on 6m" (not (Player.can_ron p wrong_tile))

let test_can_tsumo _ =
  let p_win = create_player_with_hand 
    ["1m";"1m";"1m"; "2m";"2m";"2m"; "3m";"3m"; "3m"; "4m";"4m"; "4m"; "5m"; "5m"] in
  assert_bool "Should can tsumo" (Player.can_tsumo p_win);
  
  let p_lose = create_player_with_hand 
    ["1m";"1m";"1m"; "2m";"2m";"2m"; "3m";"3m"; "3m"; "4m";"4m"; "4m"; "5m"; "7m"] in
  assert_bool "Should not tsumo" (not (Player.can_tsumo p_lose))

let test_get_recommendations _ =
  let p_14 = create_player_with_hand 
    ["1m";"2m";"3m"; "4m";"5m";"6m"; "7m";"8m";"9m"; "1p";"2p";"3p"; "4p"; "9s"] in
  let recs = Player.get_recommendations p_14 in
  assert_bool "Should return recommendations list" (List.length recs > 0);
  
  let p_13 = create_player_with_hand 
    ["1m";"2m";"3m"; "4m";"5m";"6m"; "7m";"8m";"9m"; "1p";"2p";"3p"; "4p"] in
  let recs_empty = Player.get_recommendations p_13 in
  assert_equal 0 (List.length recs_empty) ~msg:"Should return empty list for 13 tiles"

let test_game_create _ =
  let g = Game.create () in
  assert_equal 0 (Game.current_player_id g) ~msg:"Game should start with Player 0";
  assert_equal 70 (Game.remaining_tiles g) ~msg:"Remaining tiles should be 70";
  let players = Game.all_players g in
  assert_equal 4 (List.length players);
  List.iter (fun p -> 
    assert_equal 13 (Player.tile_count p)
  ) players

let test_draw_and_discard _ =
  let g = Game.create () in
  
  let (g_drawn, tile_opt) = Game.draw_card g in
  assert_bool "Draw should return a tile" (tile_opt <> None);
  
  let p_drawn = Game.current_player g_drawn in
  assert_equal 14 (Player.tile_count p_drawn) ~msg:"Hand size should be 14 after draw";
  assert_equal 69 (Game.remaining_tiles g_drawn) ~msg:"Deck count should decrease";
  
  let tile_to_discard = Option.get tile_opt in
  let (g_discarded, disc_opt) = Game.discard_card g_drawn tile_to_discard in
  assert_bool "Discard should return the tile" (disc_opt <> None);
  
  assert_equal 1 (Game.current_player_id g_discarded) ~msg:"Turn should rotate to Player 1"

let test_bot_step _ =
  let g = Game.create () in
  let g_p1 = Game.next_turn g in
  assert_equal 1 (Game.current_player_id g_p1);
  
  let (g_next, success) = Game.play_bot_step g_p1 in
  assert_bool "Bot step should be successful" success;
  
  assert_equal 2 (Game.current_player_id g_next) ~msg:"Turn should rotate to Player 2 after bot move";
  assert_equal 69 (Game.remaining_tiles g_next) ~msg:"Bot should have consumed 1 card"

let test_game_perform_pon_fail _ =
  let g = Game.create () in
  let target = Tile.Honor(Tile.Red) in
  let (g_new, success) = Game.perform_pon g target in
  if not success then
    assert_equal g g_new ~msg:"State should not change on failure"

let test_last_discard_logic _ =
  let g = Game.create () in
  
  assert_equal None (Game.last_discard g) ~msg:"Should have no discards at start";

  let (g_drawn, drawn_opt) = Game.draw_card g in
  let tile_to_discard = Option.get drawn_opt in
  let (g_after_discard, _) = Game.discard_card g_drawn tile_to_discard in
  
  assert_equal 1 (Game.current_player_id g_after_discard) ~msg:"Turn should rotate to Player 1";
  
  match Game.last_discard g_after_discard with
  | None -> assert_failure "Should return the last discarded tile"
  | Some t -> 
      assert_equal (Tile.to_string tile_to_discard) (Tile.to_string t) 
      ~msg:"Last discard should match the tile player 0 discarded"

let test_perform_chi_fail _ =
  let g = Game.create () in
  
  let target = Tile.Numbered(Tile.Man, 5) in
  let t1 = Tile.Honor(Tile.East) in
  let t2 = Tile.Honor(Tile.West) in
  
  let (g_new, success) = Game.perform_chi g target t1 t2 in
  
  assert_bool "Chi should fail with invalid tiles" (not success);
  assert_equal (Game.current_player_id g) (Game.current_player_id g_new)

let test_perform_kan_fail _ =
  let g = Game.create () in
  let target = Tile.Honor(Tile.Green) in
  
  let (g_new, success) = Game.perform_kan g target in
  
  assert_bool "Kan should fail without triplet" (not success);
  assert_equal 0 (Game.current_player_id g_new)

let test_winner_initial _ =
  let g = Game.create () in
  assert_equal None (Game.winner g) ~msg:"Should be no winner at start"

let setup_game_with_hand hand_strs =
  let g = Game.create () in
  let p0 = List.nth (Game.all_players g) 0 in
  let p0_custom = Player.debug_set_hand p0 (make_hand hand_strs) in
  Game.debug_set_player g 0 p0_custom

let test_game_perform_chi _ =
  let g = setup_game_with_hand ["3m"; "4m"; "1z"; "1z"] in
  
  let target = Tile.Numbered(Tile.Man, 2) in
  let t1 = Tile.Numbered(Tile.Man, 3) in
  let t2 = Tile.Numbered(Tile.Man, 4) in
  
  let (g_new, success) = Game.perform_chi g target t1 t2 in
  
  assert_bool "Game chi should succeed" success;
  
  let p0_new = Game.current_player g_new in
  assert_equal 0 (Game.current_player_id g_new) ~msg:"Turn should remain with Player 0 to discard";
  
  assert_equal 2 (List.length (Player.hand p0_new));
  
  assert_equal 1 (List.length (Player.melds p0_new))

let test_game_perform_pon _ =
  let g = setup_game_with_hand ["6z"; "6z"; "1m"] in
  
  let g_turn1 = Game.next_turn g in 
  assert_equal 1 (Game.current_player_id g_turn1);
  
  let target = Tile.Honor(Tile.Green) in 
  
  let (g_new, success) = Game.perform_pon g_turn1 target in
  
  assert_bool "Game pon should succeed" success;
  
  assert_equal 0 (Game.current_player_id g_new) ~msg:"Turn should jump back to Player 0";
  
  let p0_new = List.nth (Game.all_players g_new) 0 in
  assert_equal 1 (List.length (Player.hand p0_new))

let test_game_perform_kan _ =
  let g = setup_game_with_hand ["1z"; "1z"; "1z"; "2m"] in
  
  let g_turn2 = Game.next_turn (Game.next_turn g) in
  
  let target = Tile.Honor(Tile.East) in 
  
  let (g_new, success) = Game.perform_kan g_turn2 target in
  
  assert_bool "Game kan should succeed" success;
  
  assert_equal 0 (Game.current_player_id g_new) ~msg:"Turn should jump back to Player 0";
  
  let p0_new = List.nth (Game.all_players g_new) 0 in
  
  assert_equal 2 (List.length (Player.hand p0_new)) ~msg:"Should draw Rinshan tile (1 remaining + 1 drawn)"
let suite =
  "MahjongTests" >::: [
    "test_win_hand" >:: test_win_hand;
    "test_efficiency" >:: test_efficiency;
    "test_game_flow" >:: test_game_flow;
    "test_find_chi_options" >:: test_find_chi_options;
    "test_perform_chi" >:: test_perform_chi;  
    "test_pon" >:: test_pon;
    "test_kan" >:: test_kan;
    "test_draw_discard" >:: test_draw_discard;
    "test_to_string" >:: test_to_string;
    "test_has_full_hand" >:: test_has_full_hand;
    "test_can_ron" >:: test_can_ron;
    "test_can_tsumo" >:: test_can_tsumo;
    "test_get_recommendations" >:: test_get_recommendations;
    "test_game_create" >:: test_game_create;
    "test_draw_and_discard" >:: test_draw_and_discard;
    "test_bot_step" >:: test_bot_step;
    "test_game_perform_pon_fail" >:: test_game_perform_pon_fail;
    "test_last_discard_logic" >:: test_last_discard_logic;
    "test_perform_chi_fail" >:: test_perform_chi_fail;
    "test_perform_kan_fail" >:: test_perform_kan_fail;
    "test_winner_initial" >:: test_winner_initial;
    "test_game_perform_chi" >:: test_game_perform_chi;
    "test_game_perform_pon" >:: test_game_perform_pon;
    "test_game_perform_kan" >:: test_game_perform_kan;
  ]

let () = run_test_tt_main suite