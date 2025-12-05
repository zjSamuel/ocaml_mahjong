(* test/main_test.ml *)
open OUnit2
open Mahjong

(* --- 1. 辅助函数：把字符串转换为 Tile 列表，方便写测试用例 --- *)
(* 格式：1m=1万, 5p=5筒, 9s=9索, 1z=东, 2z=南, 3z=西, 4z=北, 5z=白, 6z=发, 7z=中 *)
(* 注意：为了简化，这里简单映射。你的 Tile.Honor 定义可能不同，请根据实际调整 *)
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
  (* 简单断言：刚开始应该是 Player 0 *)
  assert_equal 0 (Game.current_player_id g) ~msg:"Initial player should be 0";
  (* 断言：牌堆里应该有牌 *)
  assert_bool "Deck should not be empty" (Game.remaining_tiles g > 0);
  (* 断言：玩家应该有 13 张牌 *)
  let p = Game.current_player g in
  assert_equal 13 (Player.tile_count p) ~msg:"Player should start with 13 tiles"

let create_player_with_hand hand_strs =
  let p = Player.create "TestBot" in
  Player.debug_set_hand p (make_hand hand_strs)

let test_find_chi_options _ =
  (* 手牌: 2万, 3万, 5万。上家打出 1万 *)
  let p = create_player_with_hand ["2m"; "3m"; "5m"] in
  let target = Tile.Numbered(Tile.Man, 1) in
  let options = Player.find_chi_options p target in
  
  (* 应该能找到一组 (2万, 3万) *)
  assert_equal 1 (List.length options) ~msg:"Should find 1 chi option";
  let (t1, t2) = List.hd options in
  assert_equal "2万" (Tile.to_string t1);
  assert_equal "3万" (Tile.to_string t2)

(* 2. 测试执行吃 (Perform Chi) *)
let test_perform_chi _ =
  (* 手牌: 2万, 3万, 5万 *)
  let p = create_player_with_hand ["2m"; "3m"; "5m"] in
  let target = Tile.Numbered(Tile.Man, 4) in (* 吃 4万，用 2,3 或者 3,5? 不，这里测试用 3,5 吃 4 *)
  let t1 = Tile.Numbered(Tile.Man, 3) in
  let t2 = Tile.Numbered(Tile.Man, 5) in
  
  match Player.perform_chi p target t1 t2 with
  | None -> assert_failure "Perform chi failed"
  | Some new_p ->
      (* 检查手牌：应该只剩 2万 *)
      assert_equal 1 (List.length (Player.hand new_p));
      assert_equal "2万" (Tile.to_string (List.hd (Player.hand new_p)));
      (* 检查副露：应该有一组 Chi *)
      assert_equal 1 (List.length (Player.melds new_p));
      match List.hd (Player.melds new_p) with
      | Player.Chi _ -> ()
      | _ -> assert_failure "Meld type should be Chi"

(* 3. 测试碰牌 (Pon) 判定与执行 *)
let test_pon _ =
  (* 手牌: 2个白, 1个发 *)
  let p = create_player_with_hand ["5z"; "5z"; "6z"] in
  let target = Tile.Honor(Tile.White) in (* 白 *)
  
  (* 判定 *)
  assert_bool "Should be able to pon" (Player.can_pon p target);
  
  (* 执行 *)
  match Player.perform_pon p target with
  | None -> assert_failure "Perform pon failed"
  | Some new_p ->
      (* 手牌应该只剩 1 个发 (2个白被移除了) *)
      assert_equal 1 (List.length (Player.hand new_p));
      assert_equal "发" (Tile.to_string (List.hd (Player.hand new_p)));
      (* 副露应该是 Pon *)
      match List.hd (Player.melds new_p) with
      | Player.Pon _ -> ()
      | _ -> assert_failure "Meld type should be Pon"

(* 4. 测试杠牌 (Kan) 判定与执行 *)
let test_kan _ =
  (* 手牌: 3个中 *)
  let p = create_player_with_hand ["7z"; "7z"; "7z"] in
  let target = Tile.Honor(Tile.Red) in (* 中 *)
  
  assert_bool "Should be able to kan" (Player.can_kan p target);
  
  match Player.perform_kan p target with
  | None -> assert_failure "Perform kan failed"
  | Some new_p ->
      (* 手牌应该空了 *)
      assert_equal 0 (List.length (Player.hand new_p));
      match List.hd (Player.melds new_p) with
      | Player.Kan _ -> ()
      | _ -> assert_failure "Meld type should be Kan"

(* 5. 测试基础操作：摸牌与打牌 *)
let test_draw_discard _ =
  let p = Player.create "Tester" in
  let deck = Deck.create () in
  
  (* 摸牌 *)
  match Player.draw_tile p deck with
  | None -> assert_failure "Draw failed"
  | Some (p_drawn, _) ->
      assert_equal 1 (List.length (Player.hand p_drawn));
      
      (* 打牌 *)
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

(* 6. 测试 to_string 格式 *)
let test_to_string _ =
  let p = create_player_with_hand ["1m"; "2m"; "3m"] in
  let s = Player.to_string p in
  (* 检查是否包含玩家名字 *)
  assert_string_contains s "TestBot";
  (* 检查是否包含手牌信息 *)
  assert_string_contains s "1万"; 
  assert_string_contains s "2万"

(* 7. 测试 has_full_hand 和 tile_count *)
let test_has_full_hand _ =
  (* 13 张牌 *)
  let p_13 = create_player_with_hand 
    ["1m"; "1m"; "1m"; "2m"; "2m"; "2m"; "3m"; "3m"; "3m"; "4m"; "4m"; "4m"; "5m"] in
  assert_equal 13 (Player.tile_count p_13);
  assert_bool "Should not be full hand" (not (Player.has_full_hand p_13));

  (* 模拟摸一张牌 -> 14 张 *)
  let deck = Deck.create () in
  match Player.draw_tile p_13 deck with
  | Some (p_14, _) ->
      assert_equal 14 (Player.tile_count p_14);
      assert_bool "Should be full hand" (Player.has_full_hand p_14)
  | None -> assert_failure "Draw failed"

(* 8. 测试荣和判定 (can_ron) *)
let test_can_ron _ =
  (* 听牌: 111 222 333 444 5 (单钓 5万) *)
  let p = create_player_with_hand 
    ["1m";"1m";"1m"; "2m";"2m";"2m"; "3m";"3m"; "3m"; "4m";"4m"; "4m"; "5m"] in
  
  let win_tile = Tile.Numbered(Tile.Man, 5) in
  let wrong_tile = Tile.Numbered(Tile.Man, 7) in
  
  assert_bool "Should can ron on 5m" (Player.can_ron p win_tile);
  assert_bool "Should not ron on 6m" (not (Player.can_ron p wrong_tile))

(* 9. 测试自摸判定 (can_tsumo) *)
let test_can_tsumo _ =
  (* 已胡牌型 *)
  let p_win = create_player_with_hand 
    ["1m";"1m";"1m"; "2m";"2m";"2m"; "3m";"3m"; "3m"; "4m";"4m"; "4m"; "5m"; "5m"] in
  assert_bool "Should can tsumo" (Player.can_tsumo p_win);
  
  (* 未胡牌型 *)
  let p_lose = create_player_with_hand 
    ["1m";"1m";"1m"; "2m";"2m";"2m"; "3m";"3m"; "3m"; "4m";"4m"; "4m"; "5m"; "7m"] in
  assert_bool "Should not tsumo" (not (Player.can_tsumo p_lose))

(* 10. 测试获取牌效建议 (get_recommendations) *)
let test_get_recommendations _ =
  (* 14 张牌，应该返回建议 *)
  let p_14 = create_player_with_hand 
    ["1m";"2m";"3m"; "4m";"5m";"6m"; "7m";"8m";"9m"; "1p";"2p";"3p"; "4p"; "9s"] in
  let recs = Player.get_recommendations p_14 in
  assert_bool "Should return recommendations list" (List.length recs > 0);
  
  (* 13 张牌，不应返回建议 *)
  let p_13 = create_player_with_hand 
    ["1m";"2m";"3m"; "4m";"5m";"6m"; "7m";"8m";"9m"; "1p";"2p";"3p"; "4p"] in
  let recs_empty = Player.get_recommendations p_13 in
  assert_equal 0 (List.length recs_empty) ~msg:"Should return empty list for 13 tiles"

let test_game_create _ =
  let g = Game.create () in
  (* Check initial player *)
  assert_equal 0 (Game.current_player_id g) ~msg:"Game should start with Player 0";
  (* Check deck size: 136 - 14 (dead wall) - 52 (13*4 hands) = 70 *)
  assert_equal 70 (Game.remaining_tiles g) ~msg:"Remaining tiles should be 70";
  (* Check player hands *)
  let players = Game.all_players g in
  assert_equal 4 (List.length players);
  List.iter (fun p -> 
    assert_equal 13 (Player.tile_count p)
  ) players

let test_draw_and_discard _ =
  let g = Game.create () in
  
  (* 1. Test Draw *)
  let (g_drawn, tile_opt) = Game.draw_card g in
  assert_bool "Draw should return a tile" (tile_opt <> None);
  
  let p_drawn = Game.current_player g_drawn in
  assert_equal 14 (Player.tile_count p_drawn) ~msg:"Hand size should be 14 after draw";
  assert_equal 69 (Game.remaining_tiles g_drawn) ~msg:"Deck count should decrease";
  
  (* 2. Test Discard *)
  let tile_to_discard = Option.get tile_opt in (* Simply discard what we drew *)
  let (g_discarded, disc_opt) = Game.discard_card g_drawn tile_to_discard in
  assert_bool "Discard should return the tile" (disc_opt <> None);
  
  (* 3. Test Turn Rotation *)
  assert_equal 1 (Game.current_player_id g_discarded) ~msg:"Turn should rotate to Player 1"

let test_bot_step _ =
  let g = Game.create () in
  (* Manually rotate to a bot player (Player 1) *)
  let g_p1 = Game.next_turn g in
  assert_equal 1 (Game.current_player_id g_p1);
  
  (* Execute bot step *)
  let (g_next, success) = Game.play_bot_step g_p1 in
  assert_bool "Bot step should be successful" success;
  
  (* Bot should have drawn and discarded, so turn moves to Player 2 *)
  assert_equal 2 (Game.current_player_id g_next) ~msg:"Turn should rotate to Player 2 after bot move";
  assert_equal 69 (Game.remaining_tiles g_next) ~msg:"Bot should have consumed 1 card"

(* Test that Game.perform_pon handles failure gracefully (since we can't force a pair easily) *)
let test_game_perform_pon_fail _ =
  let g = Game.create () in
  let target = Tile.Honor(Tile.Red) in
  (* With a random hand, pon should likely fail. 
     Even if it succeeds by luck, we just check it doesn't crash. *)
  let (g_new, success) = Game.perform_pon g target in
  if not success then
    assert_equal g g_new ~msg:"State should not change on failure"

let test_last_discard_logic _ =
  let g = Game.create () in
  
  (* A. 游戏刚开始，没有人弃牌，应返回 None *)
  assert_equal None (Game.last_discard g) ~msg:"Should have no discards at start";

  (* B. 模拟玩家 0 摸牌并打牌 *)
  let (g_drawn, drawn_opt) = Game.draw_card g in
  let tile_to_discard = Option.get drawn_opt in
  let (g_after_discard, _) = Game.discard_card g_drawn tile_to_discard in
  
  (* 此时轮到玩家 1，上一张弃牌应该是玩家 0 刚刚打出的牌 *)
  assert_equal 1 (Game.current_player_id g_after_discard) ~msg:"Turn should rotate to Player 1";
  
  match Game.last_discard g_after_discard with
  | None -> assert_failure "Should return the last discarded tile"
  | Some t -> 
      assert_equal (Tile.to_string tile_to_discard) (Tile.to_string t) 
      ~msg:"Last discard should match the tile player 0 discarded"

(* 16. 测试吃牌失败逻辑 (perform_chi fail) *)
let test_perform_chi_fail _ =
  let g = Game.create () in
  (* 只有轮到自己时才能吃。我们在初始状态(Player 0)尝试吃，或者模拟轮到 Player 1 吃 *)
  (* 这里测试：Player 0 试图用两张无关的牌去吃一张牌，预期失败 *)
  
  let target = Tile.Numbered(Tile.Man, 5) in
  let t1 = Tile.Honor(Tile.East) in (* 手里大概率没有，或者无法组成顺子 *)
  let t2 = Tile.Honor(Tile.West) in
  
  let (g_new, success) = Game.perform_chi g target t1 t2 in
  
  assert_bool "Chi should fail with invalid tiles" (not success);
  (* 状态不应改变 *)
  assert_equal (Game.current_player_id g) (Game.current_player_id g_new)

(* 17. 测试杠牌失败逻辑 (perform_kan fail) *)
let test_perform_kan_fail _ =
  let g = Game.create () in
  (* Player 0 试图杠一张他手里没有 3 张的牌 *)
  let target = Tile.Honor(Tile.Green) in
  
  let (g_new, success) = Game.perform_kan g target in
  
  assert_bool "Kan should fail without triplet" (not success);
  (* 失败后，当前玩家索引不应改变 *)
  assert_equal 0 (Game.current_player_id g_new)

(* 18. 测试赢家判定 (winner) *)
let test_winner_initial _ =
  let g = Game.create () in
  (* 刚开局，大家只有 13 张牌，且是随机的，极大概率没人胡牌 *)
  (* 即使有人天胡，Game.create 也没有自动检查天胡，需要 draw 后才检查 *)
  assert_equal None (Game.winner g) ~msg:"Should be no winner at start"

let setup_game_with_hand hand_strs =
  let g = Game.create () in
  let p0 = List.nth (Game.all_players g) 0 in
  let p0_custom = Player.debug_set_hand p0 (make_hand hand_strs) in
  Game.debug_set_player g 0 p0_custom

(* 23. 测试 Game.perform_chi (吃牌流程) *)
let test_game_perform_chi _ =
  (* 1. 构造场景：玩家0 手里有 3万, 4万 *)
  (* 注意：Game.perform_chi 作用于 current_player。初始是 Player 0。 *)
  let g = setup_game_with_hand ["3m"; "4m"; "1z"; "1z"] in
  
  let target = Tile.Numbered(Tile.Man, 2) in (* 上家打出 2万 *)
  let t1 = Tile.Numbered(Tile.Man, 3) in
  let t2 = Tile.Numbered(Tile.Man, 4) in
  
  (* 2. 执行吃牌 *)
  let (g_new, success) = Game.perform_chi g target t1 t2 in
  
  (* 3. 验证成功 *)
  assert_bool "Game chi should succeed" success;
  
  (* 4. 验证状态：手牌应该变少，且仍然是 Player 0 的回合 (因为吃牌后要打牌) *)
  let p0_new = Game.current_player g_new in
  assert_equal 0 (Game.current_player_id g_new) ~msg:"Turn should remain with Player 0 to discard";
  
  (* 3万, 4万 没了，剩下 1z, 1z (2张) *)
  assert_equal 2 (List.length (Player.hand p0_new));
  
  (* 验证副露增加 *)
  assert_equal 1 (List.length (Player.melds p0_new))

(* 24. 测试 Game.perform_pon (碰牌流程) *)
let test_game_perform_pon _ =
  (* 1. 构造场景：玩家0 手里有两张 发财 *)
  let g = setup_game_with_hand ["6z"; "6z"; "1m"] in
  
  (* 模拟现在是 Player 1 的回合 (机器人刚打完牌) *)
  let g_turn1 = Game.next_turn g in 
  assert_equal 1 (Game.current_player_id g_turn1);
  
  let target = Tile.Honor(Tile.Green) in (* 别人打出 发财 *)
  
  (* 2. 玩家0 执行全局碰牌 *)
  let (g_new, success) = Game.perform_pon g_turn1 target in
  
  (* 3. 验证成功 *)
  assert_bool "Game pon should succeed" success;
  
  (* 4. [关键] 验证抢回合：回合应该强制跳回 Player 0 *)
  assert_equal 0 (Game.current_player_id g_new) ~msg:"Turn should jump back to Player 0";
  
  let p0_new = List.nth (Game.all_players g_new) 0 in
  (* 手牌应剩 1m (1张) *)
  assert_equal 1 (List.length (Player.hand p0_new))

(* 25. 测试 Game.perform_kan (杠牌流程) *)
let test_game_perform_kan _ =
  (* 1. 构造场景：玩家0 手里有三张 东风 *)
  let g = setup_game_with_hand ["1z"; "1z"; "1z"; "2m"] in
  
  (* 模拟 Player 2 的回合 *)
  let g_turn2 = Game.next_turn (Game.next_turn g) in
  
  let target = Tile.Honor(Tile.East) in (* 别人打出 东风 *)
  
  (* 2. 执行杠 *)
  let (g_new, success) = Game.perform_kan g_turn2 target in
  
  (* 3. 验证成功 *)
  assert_bool "Game kan should succeed" success;
  
  (* 4. 验证回合抢夺 *)
  assert_equal 0 (Game.current_player_id g_new) ~msg:"Turn should jump back to Player 0";
  
  (* 5. 验证岭上开花逻辑 (自动摸了一张牌) *)
  let p0_new = List.nth (Game.all_players g_new) 0 in
  
  (* 初始4张 - 3张杠掉 + 1张岭上牌 = 2张手牌 *)
  (* 如果没摸岭上牌，这里会是 1 张 *)
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