open Core
open OUnit2
open Mahjong

(* ========================================== *)
(* Helpers *)
(* ========================================== *)

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

(* 快速断言某个役种存在的辅助函数 *)
let assert_has_yaku ?(msg="") result target_yaku =
  let found = List.exists result.Hand.Score.yaku_list ~f:(fun y -> 
    match (y, target_yaku) with
    | (Hand.Score.Yakuhai _, Hand.Score.Yakuhai _) -> true (* 简化 Yakuhai 比较 *)
    | (Hand.Score.Dora _, Hand.Score.Dora _) -> true
    | (a, b) -> Poly.(=) a b
  ) in
  if not found then
    let yaku_strs = List.map result.Hand.Score.yaku_list ~f:(fun _ -> "Yaku") |> String.concat ~sep:", " in
    assert_failure (Printf.sprintf "%s Expected yaku not found. Got: [%s]" msg yaku_strs)

let assert_score ?(melds=[]) ?(dora=[]) hand check_fn = 
  match Hand.calculate_score hand melds dora Tile.East Tile.South true false with
  | None -> assert_failure "Failed to calculate score for valid hand"
  | Some res -> check_fn res

(* ========================================== *)
(* 1. Basic Utility Tests *)
(* ========================================== *)

let test_utils _ =
  let t1 = Tile.Numbered(Tile.Man, 1) in
  let hand = [t1; Tile.Numbered(Tile.Man, 2)] in
  
  (* Test to_string *)
  assert_bool "to_string check" (String.length (Hand.to_string hand) > 0);
  assert_equal "(empty)" (Hand.to_string []);
  
  (* Test remove_first *)
  match Hand.remove_first hand t1 with
  | Some rest -> assert_equal 1 (List.length rest)
  | None -> assert_failure "Should remove tile"

(* ========================================== *)
(* 2. Standard Yaku Tests (Structural) *)
(* ========================================== *)

let test_pinfu _ =
  (* 234m 345p 678s 234s 99p(Pair) - No honors, all seqs *)
  let hand = make_hand ["2m";"3m";"4m"; "3p";"4p";"5p"; "6s";"7s";"8s"; "2s";"3s";"4s"; "9p";"9p"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.Pinfu)

let test_iipeiko _ =
  (* 223344m (Iipeiko) + others *)
  let hand = make_hand ["2m";"2m";"3m";"3m";"4m";"4m"; "1p";"2p";"3p"; "9s";"9s";"9s"; "1z";"1z"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.Iipeiko)

let test_ryanpeiko _ =
  (* 223344m + 223344p + 99s *)
  let hand = make_hand ["2m";"2m";"3m";"3m";"4m";"4m"; "2p";"2p";"3p";"3p";"4p";"4p"; "9s";"9s"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.Ryanpeiko)

let test_toitoi _ =
  (* All triplets (can be open) *)
  let hand = make_hand ["1m";"1m";"1m"; "9p";"9p"] in
  let melds = [
    Hand.Pon(parse_tile_str "2s", parse_tile_str "2s", parse_tile_str "2s");
    Hand.Pon(parse_tile_str "5z", parse_tile_str "5z", parse_tile_str "5z");
    Hand.Pon(parse_tile_str "8m", parse_tile_str "8m", parse_tile_str "8m");
  ] in
  assert_score ~melds hand (fun res -> assert_has_yaku res Hand.Score.Toitoi)

let test_sanankou _ =
  (* 3 concealed triplets + 1 sequence + pair *)
  let hand = make_hand ["1m";"1m";"1m"; "9p";"9p";"9p"; "2s";"2s";"2s"; "4m";"5m";"6m"; "1z";"1z"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.Sanankou)

let test_chiitoitsu _ =
  (* 7 Pairs *)
  let hand = make_hand ["1m";"1m"; "9m";"9m"; "1p";"1p"; "9p";"9p"; "1s";"1s"; "9s";"9s"; "1z";"1z"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.Chiitoitsu)

(* ========================================== *)
(* 3. Pattern Yaku Tests (Sanshoku, Itsu) *)
(* ========================================== *)

let test_sanshoku _ =
  (* 123m 123p 123s + ... *)
  let hand = make_hand ["1m";"2m";"3m"; "1p";"2p";"3p"; "1s";"2s";"3s"; "9m";"9m";"9m"; "1z";"1z"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.Sanshoku)

let test_sanshoku_doukou _ =
  (* 222m 222p 222s (Triplets) *)
  let hand = make_hand ["2m";"2m";"2m"; "2p";"2p";"2p"; "2s";"2s";"2s"; "9m";"9m";"9m"; "1z";"1z"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.SanshokuDoukou)

let test_itsu _ =
  (* 123 456 789 same suit *)  (* Note: 9z is invalid in parser usually, using 7z (Red) for safety *)
  let hand = make_hand ["1m";"2m";"3m"; "4m";"5m";"6m"; "7m";"8m";"9m"; "1p";"1p";"1p"; "7z";"7z"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.Itsu)

(* ========================================== *)
(* 4. Terminal/Honor Yaku Tests *)
(* ========================================== *)

let test_chanta _ =
  (* Mixed Triplets/Seqs, all contain 1/9/Honor *)
  let hand = make_hand ["1m";"2m";"3m"; "7p";"8p";"9p"; "1s";"1s";"1s"; "1z";"1z";"1z"; "2z";"2z"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.Chanta)

let test_junchan _ =
  (* Pure Terminals, no Honors *)
  let hand = make_hand ["1m";"2m";"3m"; "7p";"8p";"9p"; "1s";"1s";"1s"; "9s";"9s";"9s"; "1p";"1p"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.Junchan)

let test_honroutou _ =
  (* All Terminals/Honors (All Triplets/Pairs) *)
  let hand = make_hand ["1m";"1m";"1m"; "9p";"9p";"9p"; "1s";"1s";"1s"; "1z";"1z";"1z"; "2z";"2z"] in
  assert_score hand (fun res -> 
    assert_has_yaku res Hand.Score.Honroutou;
    assert_has_yaku res Hand.Score.Toitoi) (* Usually comes with Toitoi or Chiitoi *)

let test_shousangen _ =
  (* Small Three Dragons: 2 triplets + 1 pair of dragons *)
  (* 5z(White), 6z(Green), 7z(Red) *)
  let hand = make_hand ["5z";"5z";"5z"; "6z";"6z";"6z"; "7z";"7z"; "1m";"2m";"3m"; "9p";"9p";"9p"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.Shousangen)

(* ========================================== *)
(* 5. Suit Yaku Tests *)
(* ========================================== *)

let test_chinitsu _ =
  (* All Manzu *)
  let hand = make_hand ["1m";"1m";"1m"; "2m";"3m";"4m"; "5m";"5m";"5m"; "7m";"8m";"9m"; "9m";"9m"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.Chinitsu)

let test_honitsu _ =
  (* Manzu + Honors *)
  let hand = make_hand ["1m";"1m";"1m"; "1z";"1z";"1z"; "2z";"2z";"2z"; "3z";"3z"; "3z"; "4z"; "4z"] in
  assert_score hand (fun res -> assert_has_yaku res Hand.Score.Honitsu)

(* ========================================== *)
(* 6. Dora & Situational Tests *)
(* ========================================== *)

let test_dora_score _ =
  (* Hand with 3x 1m. Indicator is 9m -> Dora is 1m. *)
  let hand = make_hand ["1m";"1m";"1m"; "2p";"3p";"4p"; "5s";"6s";"7s"; "1z";"1z";"1z"; "2z";"2z"] in
  let indicators = make_hand ["9m"] in
  assert_score ~dora:indicators hand (fun res -> 
    let dora_count = List.fold res.yaku_list ~init:0 ~f:(fun acc y -> 
      match y with Hand.Score.Dora n -> acc + n | _ -> acc) in
    assert_equal 3 dora_count ~msg:"Should have 3 Dora"
  )

(* ========================================== *)
(* 7. A* Efficiency & Shanten Corner Cases *)
(* ========================================== *)

let test_efficiency_complex _ =
  (* A hand with many options. 1-shanten. *)
  (* 23m 56p 88s 11z + isolated tiles *)
  let hand = make_hand ["2m";"3m"; "5p";"6p"; "8s";"8s"; "1z";"1z"; "1m"; "9p"; "2s"; "5z"; "7z"; "4m"] in
  let visible = Array.create ~len:34 0 in
  
  let recs = Hand.calculate_efficiency hand visible in
  assert_bool "Should return recommendations" (List.length recs > 0);
  
  let (tile, ukeire) = List.hd_exn recs in
  Printf.printf "Best discard: %s, Ukeire: %d\n" (Tile.to_string tile) ukeire;
  assert_bool "Ukeire > 0" (ukeire > 0)

let test_shanten_impossible _ =
  (* 14 single tiles, different types (Kokushi attempt but incomplete) *)
  let hand = make_hand ["1m";"9m";"1p";"9p";"1s";"9s";"1z";"2z";"3z";"4z";"5z";"6z";"7z";"2m"] in
  let shanten = Hand.calculate_shanten hand in
  (* Standard should be high *)
  assert_bool "High shanten" (shanten >= 3)

(* ========================================== *)
(* Suite Definition *)
(* ========================================== *)

let suite =
  "HandCoverageTests" >::: [
    "test_utils" >:: test_utils;
    "test_pinfu" >:: test_pinfu;
    "test_iipeiko" >:: test_iipeiko;
    "test_ryanpeiko" >:: test_ryanpeiko;
    "test_toitoi" >:: test_toitoi;
    "test_sanankou" >:: test_sanankou;
    "test_chiitoitsu" >:: test_chiitoitsu;
    "test_sanshoku" >:: test_sanshoku;
    "test_sanshoku_doukou" >:: test_sanshoku_doukou;
    "test_itsu" >:: test_itsu;
    "test_chanta" >:: test_chanta;
    "test_junchan" >:: test_junchan;
    "test_honroutou" >:: test_honroutou;
    "test_shousangen" >:: test_shousangen;
    "test_chinitsu" >:: test_chinitsu;
    "test_honitsu" >:: test_honitsu;
    "test_dora_score" >:: test_dora_score;
    "test_efficiency_complex" >:: test_efficiency_complex;
    "test_shanten_impossible" >:: test_shanten_impossible;
  ]

let () = run_test_tt_main suite