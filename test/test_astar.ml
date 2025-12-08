open Core
open OUnit2
open Mahjong

(* 辅助函数：解析牌字符串 (适配 Core) *)
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
        | 1 -> Tile.East
        | 2 -> Tile.South
        | 3 -> Tile.West
        | 4 -> Tile.North
        | 5 -> Tile.White
        | 6 -> Tile.Green
        | 7 -> Tile.Red
        | _ -> failwith "Invalid honor"
      in
      Tile.Honor h
  | _ -> failwith "Invalid suit"

let make_hand str_list = List.map str_list ~f:parse_tile_str
let empty_visible_counts = Array.create ~len:34 0

(* 测试 1: 七对子 (Seven Pairs) - 确保 A* 能处理特殊牌型 *)
let test_seven_pairs _ =
  let hand =
    make_hand
      [
        "1m";
        "1m";
        "2m";
        "2m";
        "3m";
        "3m";
        "4m";
        "4m";
        "5m";
        "5m";
        "6m";
        "6m";
        "1p";
        "2p";
      ]
  in
  let recs = Hand.get_recommendations_astar hand empty_visible_counts in
  let best_discard, score = List.hd_exn recs in

  Printf.printf "\n[Seven Pairs] Best: %s, Ukeire: %d\n"
    (Tile.to_string best_discard)
    score;

  (* 应该切掉 1p 或 2p 这种单张牌 *)
  match best_discard with
  | Tile.Numbered (Tile.Pin, n) when n = 1 || n = 2 ->
      assert_bool "Score should be valid" (score > 0)
  | _ ->
      assert_failure
        (Printf.sprintf "Should discard isolated tile, got %s"
           (Tile.to_string best_discard))

(* 测试 2: 标准牌型 - 简单的进张判断 *)
let test_standard_hand _ =
  let hand =
    make_hand
      [
        "2m";
        "3m";
        "4s";
        "5s";
        "6s";
        "7s";
        "8s";
        "9s";
        "1z";
        "1z";
        "2z";
        "2z";
        "9p";
        "3z";
      ]
  in
  let recs = Hand.get_recommendations_astar hand empty_visible_counts in
  let best, _ = List.hd_exn recs in

  (* 9p 和 3z 是孤张，切掉它们进张最广 *)
  match best with
  | Tile.Honor Tile.West | Tile.Numbered (Tile.Pin, 9) -> ()
  | _ ->
      assert_failure
        (Printf.sprintf "A* failed, suggested %s" (Tile.to_string best))

(* 测试 3: 清一色复杂牌型 (Chinitsu) - 性能与正确性 *)
let test_chinitsu _ =
  let hand =
    make_hand
      [
        "1m";
        "1m";
        "1m";
        "2m";
        "3m";
        "4m";
        "5m";
        "6m";
        "7m";
        "8m";
        "9m";
        "9m";
        "9m";
        "1z";
      ]
  in
  let recs = Hand.get_recommendations_astar hand empty_visible_counts in
  let best, ukeire = List.hd_exn recs in

  assert_equal "东" (Tile.to_string best)
    ~msg:"Should discard Honor (1z) to reach Tenpai";
  assert_bool "Ukeire should be huge (9-sided wait)" (ukeire >= 20)

(* 新增测试 4: 剩余牌 heuristic 验证 *)
(* 验证当手牌非常糟糕时，A* 仍能给出合理的“最差中的最好”建议 *)
let test_garbage_hand _ =
  let hand =
    make_hand
      [
        "1m";
        "4p";
        "7s";
        "1z";
        "2z";
        "3z";
        "4z";
        "5z";
        "6z";
        "7z";
        "2m";
        "5p";
        "8s";
        "1m";
      ]
  in
  let recs = Hand.get_recommendations_astar hand empty_visible_counts in
  (* 任何建议都行，只要程序不崩，且有输出 *)
  assert_bool "Should return recommendations even for garbage"
    (List.length recs > 0)

let suite =
  "AStarTests"
  >::: [
         "test_seven_pairs" >:: test_seven_pairs;
         "test_standard_hand" >:: test_standard_hand;
         "test_chinitsu" >:: test_chinitsu;
         "test_garbage_hand" >:: test_garbage_hand;
       ]

let () = run_test_tt_main suite
