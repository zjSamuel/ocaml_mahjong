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

  match best_discard with
  | Tile.Numbered (Tile.Pin, n) when n = 1 || n = 2 ->
      assert_bool "Score should be valid" (score > 0)
  | _ ->
      assert_failure
        (Printf.sprintf "Should discard isolated tile, got %s"
           (Tile.to_string best_discard))

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

  match best with
  | Tile.Honor Tile.West | Tile.Numbered (Tile.Pin, 9) -> ()
  | _ ->
      assert_failure
        (Printf.sprintf "A* failed, suggested %s" (Tile.to_string best))

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
