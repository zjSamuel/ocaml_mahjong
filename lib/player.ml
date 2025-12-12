(* lib/player.ml *)
open Core

type difficulty =
  | Easy  (** Random discard *)
  | Medium  (** Pure A* (Efficiency only) *)
  | Hard  (** Enhanced A* (Efficiency + Score Potential) *)
[@@deriving compare, sexp]

type t = {
  name : string;
  hand : Hand.t;
  discards : Tile.t list;
  last_drawn : Tile.t option;
  melds : Hand.meld list;
  difficulty : difficulty;
}
let tiles_of_meld = function
  | Hand.Chi (a, b, c) -> [ a; b; c ]
  | Hand.Pon (a, _, _) -> [ a; a; a ]
  | Hand.Kan (a, _, _, _) -> [ a; a; a; a ]

let full_tiles (p : t) : Tile.t list =
  let meld_tiles = List.concat_map p.melds ~f:tiles_of_meld in
  p.hand @ meld_tiles


(* ========================================== *)
(* 1. Initialization and Accessors *)
(* ========================================== *)

let create name =
  {
    name;
    hand = Hand.empty;
    discards = [];
    last_drawn = None;
    melds = [];
    difficulty = Medium;
  }

let difficulty p = p.difficulty
let set_difficulty p d = { p with difficulty = d }
let name p = p.name
let hand p = p.hand
let discards p = p.discards
let last_drawn p = p.last_drawn
let melds p = p.melds
let sort_tiles tiles = List.sort tiles ~compare:Tile.compare

let tile_count p =
  let hand_n = List.length p.hand in
  let meld_n = List.length p.melds in
  hand_n + (meld_n * 3)

let has_full_hand p = tile_count p >= 14

(* ========================================== *)
(* 2. Rule Validation *)
(* ========================================== *)

let is_sequence t1 t2 t3 =
  match (t1, t2, t3) with
  | Tile.Numbered (s1, n1), Tile.Numbered (s2, n2), Tile.Numbered (s3, n3) -> (
      Tile.equal_suit s1 s2 && Tile.equal_suit s2 s3
      &&
      let sorted = List.sort [ n1; n2; n3 ] ~compare:Int.compare in
      match sorted with [ a; b; c ] -> a + 1 = b && b + 1 = c | _ -> false)
  | _ -> false

let find_chi_options p target =
  match target with
  | Tile.Honor _ -> []
  | _ ->
      let uniq_hand = List.dedup_and_sort p.hand ~compare:Tile.compare in
      let rec find acc = function
        | [] -> acc
        | h1 :: t ->
            let rec pair_with_h1 inner_acc = function
              | [] -> inner_acc
              | h2 :: t2 ->
                  if is_sequence h1 h2 target then
                    pair_with_h1 ((h1, h2) :: inner_acc) t2
                  else pair_with_h1 inner_acc t2
            in
            find (pair_with_h1 acc t) t
      in
      find [] uniq_hand

let can_pon p target =
  let count = List.count p.hand ~f:(fun t -> Tile.compare t target = 0) in
  count >= 2

let can_kan p target =
  let count = List.count p.hand ~f:(fun t -> Tile.compare t target = 0) in
  count >= 3

let can_ron p target =
  (* 1. 获取所有牌（手中暗牌 + 副露牌） *)
  let all_tiles = full_tiles p in
  (* 2. 将荣和的目标牌加入到所有牌中 *)
  let final_tiles = Hand.add all_tiles target in
  (* 3. 检查这 14 张牌是否构成和牌形状 *)
  Hand.is_complete final_tiles

let can_tsumo p =
  let all_tiles = full_tiles p in
  Hand.is_complete all_tiles
(* ========================================== *)
(* 3. Core Actions *)
(* ========================================== *)

let perform_chi p target t1 t2 =
  if not (is_sequence t1 t2 target) then None
  else
    match Hand.remove_first p.hand t1 with
    | None -> None
    | Some h1 -> (
        match Hand.remove_first h1 t2 with
        | None -> None
        | Some h2 ->
            let sorted = sort_tiles [ t1; t2; target ] in
            let m =
              match sorted with
              | [ a; b; c ] -> Hand.Chi (a, b, c)
              | _ -> Hand.Chi (t1, t2, target)
              (* Fallback *)
            in
            Some { p with hand = h2; melds = m :: p.melds; last_drawn = None })

let perform_pon p target =
  match Hand.remove_first p.hand target with
  | None -> None
  | Some h1 -> (
      match Hand.remove_first h1 target with
      | None -> None
      | Some h2 ->
          let m = Hand.Pon (target, target, target) in
          Some { p with hand = h2; melds = m :: p.melds; last_drawn = None })

let perform_kan p target =
  (* Helper to remove 3 copies *)
  let rec remove_3 h count acc =
    if count = 0 then Some (List.rev acc @ h)
    else
      match h with
      | [] -> None
      | x :: xs ->
          if Tile.compare x target = 0 then remove_3 xs (count - 1) acc
          else remove_3 xs count (x :: acc)
  in
  match remove_3 p.hand 3 [] with
  | None -> None
  | Some new_hand ->
      let m = Hand.Kan (target, target, target, target) in
      Some { p with hand = new_hand; melds = m :: p.melds; last_drawn = None }

let draw_tile p deck =
  match Deck.draw deck with
  | None -> None
  | Some (tile, next_deck) ->
      Some
        ( { p with hand = Hand.add p.hand tile; last_drawn = Some tile },
          next_deck )

let discard_tile p tile =
  match Hand.remove_first p.hand tile with
  | None -> None
  | Some new_hand ->
      Some
        {
          p with
          hand = new_hand;
          discards = tile :: p.discards;
          last_drawn = None;
        }

let add_drawn_tile p tile =
  { p with hand = Hand.add p.hand tile; last_drawn = Some tile }

let to_string p =
  let melds_str =
    List.map p.melds ~f:(function
      | Hand.Chi (a, b, c) ->
          Printf.sprintf "[Chi %s%s%s]" (Tile.to_string a) (Tile.to_string b)
            (Tile.to_string c)
      | Pon (a, _, _) -> Printf.sprintf "[Pon %s]" (Tile.to_string a)
      | Kan (a, _, _, _) -> Printf.sprintf "[Kan %s]" (Tile.to_string a))
    |> String.concat ~sep:" "
  in
  Printf.sprintf "[%s] Hand:%s Melds:%s" p.name (Hand.to_string p.hand)
    melds_str

let debug_set_hand p tiles =
  { p with hand = tiles; melds = []; last_drawn = None }

(* ========================================================== *)
(* AI Heuristics: Static Evaluation Functions                 *)
(* ========================================================== *)

let is_terminal_or_honor = function
  | Tile.Honor _ -> true
  | Tile.Numbered (_, n) -> n = 1 || n = 9

(* 1. Dora Value: Count number of doras in hand *)
let eval_dora hand dora_indicators =
  let doras = List.map dora_indicators ~f:Tile.next_dora in
  List.fold hand ~init:0 ~f:(fun acc t ->
      let matches = List.count doras ~f:(fun d -> Tile.compare t d = 0) in
      acc + matches)

(* 2. Tanyao Potential: Prefer hands with fewer terminals/honors *)
let eval_tanyao hand =
  let terminals = List.filter hand ~f:is_terminal_or_honor in
  let count = List.length terminals in
  if count = 0 then 2.0 (* Pure Tanyao *)
  else if count <= 2 then 1.0 (* Likely Tanyao *)
  else 0.0

(* 3. Yakuhai Potential: Bonus for dragon/wind pairs or triplets *)
let eval_yakuhai hand =
  let counts = Array.create ~len:7 0 in
  List.iter hand ~f:(function
    | Tile.Honor h ->
        let idx =
          match h with
          | Tile.East -> 0
          | Tile.South -> 1
          | Tile.West -> 2
          | Tile.North -> 3
          | Tile.Red -> 4
          | Tile.Green -> 5
          | Tile.White -> 6
        in
        counts.(idx) <- counts.(idx) + 1
    | _ -> ());
  let score = ref 0.0 in
  (* Check Dragons (Indices 4, 5, 6) *)
  for i = 4 to 6 do
    if counts.(i) >= 3 then score := !score +. 2.0 (* Triplet *)
    else if counts.(i) >= 2 then score := !score +. 1.0 (* Pair *)
  done;
  !score

(* 4. Honitsu (Half Flush) Potential: Bonus if one suit dominates *)
let eval_honitsu hand =
  let m_count =
    List.count hand ~f:(function
      | Tile.Numbered (Tile.Man, _) -> true
      | _ -> false)
  in
  let p_count =
    List.count hand ~f:(function
      | Tile.Numbered (Tile.Pin, _) -> true
      | _ -> false)
  in
  let s_count =
    List.count hand ~f:(function
      | Tile.Numbered (Tile.Sou, _) -> true
      | _ -> false)
  in
  let max_suit = Int.max m_count (Int.max p_count s_count) in
  (* Start giving bonuses if > 8 tiles of same suit *)
  if max_suit >= 8 then Float.of_int (max_suit - 7) *. 0.5 else 0.0

(* Comprehensive Evaluation: Speed (from A* ) + Value (from here) *)
let evaluate_hand_potential hand dora_inds =
  let score = ref 0.0 in
  let w_dora = 1.5 in
  let w_tanyao = 1.0 in
  let w_yakuhai = 1.0 in
  let w_honitsu = 1.0 in

  score := !score +. (Float.of_int (eval_dora hand dora_inds) *. w_dora);
  score := !score +. (eval_tanyao hand *. w_tanyao);
  score := !score +. (eval_yakuhai hand *. w_yakuhai);
  score := !score +. (eval_honitsu hand *. w_honitsu);
  !score

(* ========================================================== *)
(* AI Decision Implementation                                 *)
(* ========================================================== *)

(** [Pure Efficiency] Uses Hand.get_recommendations_astar to find discards with
    max Ukeire. *)
let get_recommendations_pure p visible_counts =
  if has_full_hand p then Hand.get_recommendations_astar p.hand visible_counts
  else []

(** [Enhanced AI] Combines Efficiency (Speed) with Static Evaluation (Score
    Potential). *)
let get_recommendations_enhanced p visible_counts dora_indicators =
  if has_full_hand p then
    (* 1. Get efficient candidates *)
    let candidates = Hand.get_recommendations_astar p.hand visible_counts in

    (* 2. Score candidates based on future potential *)
    let weighted_candidates =
      List.map candidates ~f:(fun (tile_to_discard, ukeire) ->
          (* Simulate discarding this tile *)
          let hand_after_discard =
            match Hand.remove_first p.hand tile_to_discard with
            | Some h -> h
            | None -> p.hand
          in

          let potential_score =
            evaluate_hand_potential hand_after_discard dora_indicators
          in

          (* Final Score = Ukeire + Potential *)
          let final_score = Float.of_int ukeire +. potential_score in

          (tile_to_discard, ukeire, final_score))
    in

    (* 3. Sort by final score descending *)
    List.sort weighted_candidates ~compare:(fun (_, _, s1) (_, _, s2) ->
        Float.compare s2 s1)
  else []


let decide_discard (p : t) (visible_counts : int array)
    (dora_indicators : Tile.t list) : Tile.t option =
  if not (has_full_hand p) then None
  else
    match p.difficulty with
    | Easy ->
        (* Random discard *)
        let n = List.length p.hand in
        if n = 0 then None else Some (List.nth_exn p.hand (Random.int n))
    | Medium -> (
        (* Pure A* *)
        match get_recommendations_pure p visible_counts with
        | (t, _) :: _ -> Some t
        | [] ->
            let n = List.length p.hand in
            if n = 0 then None else Some (List.nth_exn p.hand (Random.int n)))
    | Hard -> (
        (* Enhanced A* *)
        match get_recommendations_enhanced p visible_counts dora_indicators with
        | (t, _, _) :: _ -> Some t
        | [] -> (
            (* Fallback to Medium *)
            match get_recommendations_pure p visible_counts with
            | (t, _) :: _ -> Some t
            | [] ->
                let n = List.length p.hand in
                if n = 0 then None
                else Some (List.nth_exn p.hand (Random.int n))))
