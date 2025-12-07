open OUnit2
open Mahjong

(* 辅助函数：解析牌字符串 *)
let parse_tile_str s =
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

(* 创建一个空的可见牌数组，模拟"刚开局，场上一张牌都没有"的情况 *)
let empty_visible_counts = Array.make 34 0

(* 测试 1: 七对子 (Seven Pairs) *)
let test_seven_pairs _ =
  let hand = make_hand [
    "1m";"1m"; "2m";"2m"; "3m";"3m"; "4m";"4m"; 
    "5m";"5m"; "6m";"6m"; "1p";"2p"
  ] in
  
  (* 传入 empty_visible_counts *)
  let recs = Hand.get_recommendations_astar hand empty_visible_counts in
  let (best_discard, score) = List.hd recs in
  
  Printf.printf "\n[Seven Pairs Test] Best discard: %s, Ukeire: %d\n" 
    (Tile.to_string best_discard) score;

  match best_discard with
  | Tile.Numbered(Tile.Pin, n) when n = 1 || n = 2 -> 
      assert_bool "Score should be valid" (score > 0)
  | _ -> 
      assert_failure (Printf.sprintf "Should suggest discarding isolated tile in 7-pairs, but got %s" (Tile.to_string best_discard))

(* 测试 2: 标准牌型的一致性 *)
let test_standard_hand _ =
  let hand = make_hand [
    "2m";"3m"; "4s";"5s";"6s"; "7s";"8s";"9s"; 
    "1z";"1z"; "2z";"2z"; "9p"; "3z"
  ] in
  (* 传入 empty_visible_counts *)
  let recs = Hand.get_recommendations_astar hand empty_visible_counts in
  let (best, _) = List.hd recs in
  
  match best with
  | Tile.Honor(Tile.West) | Tile.Numbered(Tile.Pin, 9) -> () 
  | _ -> assert_failure "A* failed to identify obvious discard"

(* 测试 3: 清一色复杂牌型 (Chinitsu) *)
let test_chinitsu _ =
  let hand = make_hand [
    "1m";"1m";"1m"; "2m";"3m";"4m"; "5m";"6m"; "7m";"8m";"9m";"9m";"9m"; "1z"
  ] in
  (* 传入 empty_visible_counts *)
  let recs = Hand.get_recommendations_astar hand empty_visible_counts in
  let (best, ukeire) = List.hd recs in
  
  assert_equal "东" (Tile.to_string best) ~msg:"Should discard Honor to reach Tenpai";
  assert_bool "Ukeire should be huge (9-sided wait)" (ukeire > 20)

let suite =
  "AStarTests" >::: [
    "test_seven_pairs" >:: test_seven_pairs;
    "test_standard_hand" >:: test_standard_hand;
    "test_chinitsu" >:: test_chinitsu;
  ]

let () = run_test_tt_main suite