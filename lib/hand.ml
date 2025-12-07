(* lib/hand.ml *)

(* ========================================== *)
(* 1. 基础类型与工具 (Basic Types & Utils) *)
(* ========================================== *)

type meld =
  | Chi of Tile.t * Tile.t * Tile.t
  | Pon of Tile.t * Tile.t * Tile.t
  | Kan of Tile.t * Tile.t * Tile.t * Tile.t

type t = Tile.t list

let empty = []
let sort hand = List.sort Tile.compare hand
let add hand tile = sort (tile :: hand)

let to_string hand =
  if hand = [] then "(empty)"
  else hand |> List.map Tile.to_string |> String.concat " "

let remove_first hand tile =
  let rec aux acc = function
    | [] -> None
    | h :: t ->
        if Tile.compare h tile = 0 then Some (List.rev acc @ t)
        else aux (h :: acc) t
  in
  aux [] hand

let tile_to_id = function
  | Tile.Numbered (Tile.Man, n) -> n - 1
  | Tile.Numbered (Tile.Pin, n) -> 9 + (n - 1)
  | Tile.Numbered (Tile.Sou, n) -> 18 + (n - 1)
  | Tile.Honor h -> (
      27
      +
      match h with
      | Tile.East -> 0
      | Tile.South -> 1
      | Tile.West -> 2
      | Tile.North -> 3
      | Tile.Red -> 4
      | Tile.Green -> 5
      | Tile.White -> 6)

let all_tile_types =
  let suits = [ Tile.Man; Tile.Pin; Tile.Sou ] in
  let nums = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  let honors =
    [
      Tile.East;
      Tile.South;
      Tile.West;
      Tile.North;
      Tile.Red;
      Tile.Green;
      Tile.White;
    ]
  in
  let numbered =
    List.concat_map
      (fun s -> List.map (fun n -> Tile.Numbered (s, n)) nums)
      suits
  in
  numbered @ List.map (fun h -> Tile.Honor h) honors

let to_frequency_table hand =
  let counts = Array.make 34 0 in
  List.iter
    (fun t ->
      let id = tile_to_id t in
      counts.(id) <- counts.(id) + 1)
    hand;
  counts

let is_terminal_or_honor = function
  | Tile.Honor _ -> true
  | Tile.Numbered (_, n) -> n = 1 || n = 9

let is_honor = function Tile.Honor _ -> true | _ -> false
let get_suit = function Tile.Numbered (s, _) -> Some s | Tile.Honor _ -> None

(* ========================================== *)
(* 2. A* 搜索逻辑 (Project Requirement: Functor) *)
(* ========================================== *)

(* 定义搜索状态，实现 Astar.Searchable 接口 *)
module DecompositionSearch = struct
  type t = { counts : int array; idx : int }

  let compare a b =
    let c = compare a.idx b.idx in
    if c <> 0 then c else compare a.counts b.counts

  (* 启发式函数: h(n) = 0 (Dijkstra) 保证找到最优解，即消耗废牌最少 *)
  let heuristic _ = 0.0
  let is_goal state = state.idx >= 34

  let neighbors state =
    if state.idx >= 34 then []
    else
      let c = state.counts.(state.idx) in
      if c == 0 then [ ({ state with idx = state.idx + 1 }, 0.0) ]
      else
        let res = ref [] in

        (* 尝试刻子 (Cost 0) *)
        if c >= 3 then (
          let next_c = Array.copy state.counts in
          next_c.(state.idx) <- c - 3;
          res := ({ state with counts = next_c }, 0.0) :: !res);

        (* 尝试顺子 (Cost 0) *)
        (if state.idx < 27 && state.idx mod 9 < 7 then
           let c2 = state.counts.(state.idx + 1) in
           let c3 = state.counts.(state.idx + 2) in
           if c >= 1 && c2 >= 1 && c3 >= 1 then (
             let next_c = Array.copy state.counts in
             next_c.(state.idx) <- c - 1;
             next_c.(state.idx + 1) <- c2 - 1;
             next_c.(state.idx + 2) <- c3 - 1;
             res := ({ state with counts = next_c }, 0.0) :: !res));

        (* 放弃当前牌 (Cost 1.0) *)
        let next_c_skip = Array.copy state.counts in
        next_c_skip.(state.idx) <- c - 1;
        res := ({ counts = next_c_skip; idx = state.idx }, 1.0) :: !res;

        !res
end

(* 使用通用库实例化求解器 *)
module Solver = Astar.Make (DecompositionSearch)

(* 贪心计算搭子 *)
let count_tatsu_greedy counts =
  let c = Array.copy counts in
  let tatsu = ref 0 in
  for i = 0 to 26 do
    while i mod 9 < 8 && c.(i) > 0 && c.(i + 1) > 0 do
      incr tatsu;
      c.(i) <- c.(i) - 1;
      c.(i + 1) <- c.(i + 1) - 1
    done;
    while i mod 9 < 7 && c.(i) > 0 && c.(i + 2) > 0 do
      incr tatsu;
      c.(i) <- c.(i) - 1;
      c.(i + 2) <- c.(i + 2) - 1
    done
  done;
  for i = 0 to 33 do
    while c.(i) >= 2 do
      incr tatsu;
      c.(i) <- c.(i) - 2
    done
  done;
  !tatsu

(* 标准向听数计算 *)
let calculate_standard_shanten counts =
  let max_score = ref (-99) in

  let check_rest current_counts has_pair =
    let start_node = { DecompositionSearch.counts = current_counts; idx = 0 } in
    match Solver.search start_node with
    | None -> ()
    | Some (min_waste, final_state) ->
        let total_tiles = Array.fold_left ( + ) 0 current_counts in
        let used_for_melds = total_tiles - int_of_float min_waste in
        let melds = used_for_melds / 3 in
        let tatsu = count_tatsu_greedy final_state.counts in
        let effective_tatsu = min tatsu (4 - melds) in
        let pair_score = if has_pair then 1 else 0 in
        let score = (melds * 2) + effective_tatsu + pair_score in
        if score > !max_score then max_score := score
  in

  for i = 0 to 33 do
    if counts.(i) >= 2 then (
      let c = Array.copy counts in
      c.(i) <- c.(i) - 2;
      check_rest c true)
  done;
  check_rest (Array.copy counts) false;
  8 - !max_score

(* 七对子向听数 *)
let calculate_chiitoitsu_shanten counts =
  let pairs = ref 0 in
  let kinds = ref 0 in
  for i = 0 to 33 do
    if counts.(i) > 0 then incr kinds;
    if counts.(i) >= 2 then incr pairs
  done;
  let shanten = 6 - !pairs in
  if !kinds < 7 then shanten + (7 - !kinds) else shanten

(* 综合向听数 *)
let calculate_shanten hand =
  let counts = to_frequency_table hand in
  min (calculate_standard_shanten counts) (calculate_chiitoitsu_shanten counts)

let is_complete hand = calculate_shanten hand <= -1

(* ========================================== *)
(* 3. 进张与切牌建议 (AI Helpers) *)
(* ========================================== *)

let possible_sets _ = [] (* Placeholder *)

let calc_ukeire hand_13 visible_counts =
  let current_shanten = calculate_shanten hand_13 in
  let effective_count = ref 0 in
  List.iter
    (fun tile ->
      let temp_hand = tile :: hand_13 in
      if calculate_shanten temp_hand < current_shanten then
        let id = tile_to_id tile in
        let seen = visible_counts.(id) in
        let possible = 4 - seen in
        if possible > 0 then effective_count := !effective_count + possible)
    all_tile_types;
  !effective_count

let get_recommendations_astar hand visible_counts =
  let base_shanten = calculate_shanten hand in
  let unique_tiles = List.sort_uniq Tile.compare hand in
  List.filter_map
    (fun tile ->
      match remove_first hand tile with
      | None -> None
      | Some hand_13 ->
          let new_shanten = calculate_shanten hand_13 in
          if new_shanten > base_shanten then None
          else
            let ukeire = calc_ukeire hand_13 visible_counts in
            if ukeire > 0 then Some (tile, ukeire) else None)
    unique_tiles
  |> List.sort (fun (_, a) (_, b) -> compare b a)

let calculate_efficiency = get_recommendations_astar

(* ========================================== *)
(* 4. 役种判定与计分 (Yaku & Scoring) *)
(* ========================================== *)

module Score = struct
  type yaku =
    | MenzenTsumo
    | Riichi
    | Ippatsu
    | Pinfu
    | Tanyao
    | Iipeiko
    | Yakuhai of string
    | Rinshan
    | Sanshoku
    | Itsu
    | Chanta
    | Chiitoitsu
    | Toitoi
    | Sanankou
    | Sankantsu
    | SanshokuDoukou
    | Honroutou
    | Shousangen
    | Honitsu
    | Junchan
    | Ryanpeiko
    | Chinitsu
    | Dora of int

  type result = { han : int; yaku_list : yaku list; fu : int; points : int }
end

type decomposition = {
  sequences : Tile.t list list;
  triplets : Tile.t list list;
  pair : Tile.t list;
}

(* 将手牌拆解为所有的 (面子+雀头) 组合 - 使用 DFS 以覆盖所有算分可能性 *)
let partition_hand (hand : t) : decomposition list =
  let counts = to_frequency_table hand in
  let results = ref [] in
  let rec solve c seqs trips pair_opt idx =
    if idx >= 34 then
      match pair_opt with
      | Some p ->
          results :=
            { sequences = seqs; triplets = trips; pair = p } :: !results
      | None -> ()
    else if c.(idx) == 0 then solve c seqs trips pair_opt (idx + 1)
    else (
      (* 尝试雀头 *)
      if pair_opt = None && c.(idx) >= 2 then (
        c.(idx) <- c.(idx) - 2;
        let tile = List.nth all_tile_types idx in
        solve c seqs trips (Some [ tile; tile ]) idx;
        c.(idx) <- c.(idx) + 2);
      (* 尝试刻子 *)
      if c.(idx) >= 3 then (
        c.(idx) <- c.(idx) - 3;
        let tile = List.nth all_tile_types idx in
        solve c seqs ([ tile; tile; tile ] :: trips) pair_opt idx;
        c.(idx) <- c.(idx) + 3);
      (* 尝试顺子 *)
      if
        idx < 27
        && idx mod 9 < 7
        && c.(idx) > 0
        && c.(idx + 1) > 0
        && c.(idx + 2) > 0
      then (
        c.(idx) <- c.(idx) - 1;
        c.(idx + 1) <- c.(idx + 1) - 1;
        c.(idx + 2) <- c.(idx + 2) - 1;
        let t1 = List.nth all_tile_types idx in
        let t2 = List.nth all_tile_types (idx + 1) in
        let t3 = List.nth all_tile_types (idx + 2) in
        solve c ([ t1; t2; t3 ] :: seqs) trips pair_opt idx;
        c.(idx) <- c.(idx) + 1;
        c.(idx + 1) <- c.(idx + 1) + 1;
        c.(idx + 2) <- c.(idx + 2) + 1))
  in
  solve counts [] [] None 0;
  !results

(* 役种检查辅助函数 *)
let get_all_groups decomp melds =
  decomp.sequences @ decomp.triplets @ [ decomp.pair ]
  @ List.map
      (function
        | Chi (a, b, c) -> [ a; b; c ]
        | Pon (a, _, _) -> [ a; a; a ]
        | Kan (a, _, _, _) -> [ a; a; a; a ])
      melds

let check_tanyao all_tiles = not (List.exists is_terminal_or_honor all_tiles)

let check_yakuhai decomp melds round_wind seat_wind =
  let check_group tiles =
    match List.hd tiles with
    | Tile.Honor h ->
        let yaku = [] in
        let yaku =
          if h = Tile.Red || h = Tile.Green || h = Tile.White then
            Score.Yakuhai (Tile.to_string (Tile.Honor h)) :: yaku
          else yaku
        in
        let yaku =
          if h = round_wind then Score.Yakuhai "场风" :: yaku else yaku
        in
        let yaku = if h = seat_wind then Score.Yakuhai "自风" :: yaku else yaku in
        yaku
    | _ -> []
  in
  let trip_yaku = List.concat_map check_group decomp.triplets in
  let meld_yaku =
    List.concat_map
      (function
        | Pon (t, _, _) | Kan (t, _, _, _) -> check_group [ t ] | _ -> [])
      melds
  in
  List.sort_uniq compare (trip_yaku @ meld_yaku)

let check_pinfu decomp melds round_wind seat_wind =
  if melds <> [] || decomp.triplets <> [] then false
  else
    match List.hd decomp.pair with
    | Tile.Honor h ->
        not
          (h = Tile.Red || h = Tile.Green || h = Tile.White || h = round_wind
         || h = seat_wind)
    | _ -> true

let count_identical_seqs seqs =
  let sorted = List.sort compare seqs in
  let rec count acc = function
    | a :: b :: rest ->
        if compare a b = 0 then count (acc + 1) rest else count acc (b :: rest)
    | _ -> acc
  in
  count 0 sorted

let check_iipeiko decomp melds =
  melds = [] && count_identical_seqs decomp.sequences = 1

let check_ryanpeiko decomp melds =
  melds = [] && count_identical_seqs decomp.sequences = 2

let check_sanshoku decomp melds =
  let all_seqs =
    decomp.sequences
    @ List.filter_map
        (function Chi (a, b, c) -> Some [ a; b; c ] | _ -> None)
        melds
  in
  let get_start_num seq =
    match List.sort Tile.compare seq with
    | Tile.Numbered (_, n) :: _ -> Some n
    | _ -> None
  in
  let get_suit seq =
    match List.hd seq with Tile.Numbered (s, _) -> Some s | _ -> None
  in
  let nums = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  List.exists
    (fun n ->
      let seqs_with_n =
        List.filter (fun s -> get_start_num s = Some n) all_seqs
      in
      let suits = List.filter_map get_suit seqs_with_n in
      List.mem Tile.Man suits && List.mem Tile.Pin suits
      && List.mem Tile.Sou suits)
    nums

let check_itsu decomp melds =
  let all_seqs =
    decomp.sequences
    @ List.filter_map
        (function Chi (a, b, c) -> Some [ a; b; c ] | _ -> None)
        melds
  in
  let check_suit s_type =
    let has n =
      List.exists
        (fun s ->
          match List.hd s with
          | Tile.Numbered (st, num) -> st = s_type && num = n
          | _ -> false)
        all_seqs
    in
    has 1 && has 4 && has 7
  in
  check_suit Tile.Man || check_suit Tile.Pin || check_suit Tile.Sou

let check_toitoi decomp = decomp.sequences = []

let check_sanankou decomp melds is_tsumo =
  let hand_trips = List.length decomp.triplets in
  if is_tsumo then hand_trips >= 3 else hand_trips >= 3

let check_sankantsu melds =
  let kans = List.filter (function Kan _ -> true | _ -> false) melds in
  List.length kans >= 3

let check_sanshoku_doukou decomp melds =
  let all_trips =
    decomp.triplets
    @ List.filter_map
        (function
          | Pon (a, _, _) -> Some [ a; a; a ]
          | Kan (a, _, _, _) -> Some [ a; a; a; a ]
          | _ -> None)
        melds
  in
  let get_num t =
    match List.hd t with Tile.Numbered (_, n) -> Some n | _ -> None
  in
  let get_suit t =
    match List.hd t with Tile.Numbered (s, _) -> Some s | _ -> None
  in
  let nums = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  List.exists
    (fun n ->
      let trips_with_n = List.filter (fun t -> get_num t = Some n) all_trips in
      let suits = List.filter_map get_suit trips_with_n in
      List.mem Tile.Man suits && List.mem Tile.Pin suits
      && List.mem Tile.Sou suits)
    nums

let check_shousangen decomp melds =
  let all_trips =
    decomp.triplets
    @ List.filter_map
        (function
          | Pon (a, _, _) -> Some [ a; a; a ]
          | Kan (a, _, _, _) -> Some [ a; a; a; a ]
          | _ -> None)
        melds
  in
  let has_dragon h =
    List.exists
      (fun t -> match List.hd t with Tile.Honor x -> x = h | _ -> false)
      all_trips
  in
  let dragons = [ Tile.White; Tile.Green; Tile.Red ] in
  let triplet_dragons = List.filter has_dragon dragons in
  let pair_dragon =
    match List.hd decomp.pair with
    | Tile.Honor h -> if List.mem h dragons then Some h else None
    | _ -> None
  in
  match pair_dragon with
  | Some p ->
      List.length triplet_dragons >= 2 && not (List.mem p triplet_dragons)
  | None -> false

let check_junchan decomp melds =
  let groups = get_all_groups decomp melds in
  List.for_all
    (fun g ->
      List.exists
        (function Tile.Numbered (_, n) -> n = 1 || n = 9 | _ -> false)
        g)
    groups

let check_honroutou decomp melds =
  let groups = get_all_groups decomp melds in
  List.for_all (fun g -> List.for_all is_terminal_or_honor g) groups

let check_chanta decomp melds =
  let groups = get_all_groups decomp melds in
  List.for_all (fun g -> List.exists is_terminal_or_honor g) groups

let check_suits all_tiles =
  let suits = List.filter_map get_suit all_tiles in
  let has_honor = List.exists is_honor all_tiles in
  let uniq_suits = List.sort_uniq compare suits in
  match (List.length uniq_suits, has_honor) with
  | 1, false -> Some Score.Chinitsu
  | 1, true -> Some Score.Honitsu
  | _ -> None

let check_chiitoitsu_hand hand =
  let counts = to_frequency_table hand in
  let pairs = ref 0 in
  for i = 0 to 33 do
    if counts.(i) = 2 then incr pairs
  done;
  !pairs = 7

let count_dora all_tiles indicators =
  let doras = List.map Tile.next_dora indicators in
  let count =
    List.fold_left
      (fun acc t ->
        let matches = List.filter (fun d -> Tile.compare t d = 0) doras in
        acc + List.length matches)
      0 all_tiles
  in
  if count > 0 then [ Score.Dora count ] else []

(* 最终算分函数 *)
let calculate_score (hand : t) (melds : meld list) (indicators : Tile.t list)
    (round_wind : Tile.honor) (seat_wind : Tile.honor) (is_tsumo : bool)
    (is_rinshan : bool) : Score.result option =
  let all_tiles =
    hand
    @ List.flatten
        (List.map
           (function
             | Chi (a, b, c) -> [ a; b; c ]
             | Pon (a, _, _) -> [ a; a; a ]
             | Kan (a, _, _, _) -> [ a; a; a; a ])
           melds)
  in
  let is_menzen = melds = [] in
  let possible_results = ref [] in

  (* 1. 标准型判定 *)
  let partitions = partition_hand hand in
  List.iter
    (fun decomp ->
      let yaku_lst = ref [] in

      if is_menzen && is_tsumo then yaku_lst := Score.MenzenTsumo :: !yaku_lst;
      if is_rinshan then yaku_lst := Score.Rinshan :: !yaku_lst;
      if check_tanyao all_tiles then yaku_lst := Score.Tanyao :: !yaku_lst;
      if check_pinfu decomp melds round_wind seat_wind then
        yaku_lst := Score.Pinfu :: !yaku_lst;
      if check_iipeiko decomp melds then yaku_lst := Score.Iipeiko :: !yaku_lst;
      yaku_lst := !yaku_lst @ check_yakuhai decomp melds round_wind seat_wind;

      if check_toitoi decomp then yaku_lst := Score.Toitoi :: !yaku_lst;
      if check_sanankou decomp melds is_tsumo then
        yaku_lst := Score.Sanankou :: !yaku_lst;
      if check_sankantsu melds then yaku_lst := Score.Sankantsu :: !yaku_lst;
      if check_sanshoku_doukou decomp melds then
        yaku_lst := Score.SanshokuDoukou :: !yaku_lst;
      if check_sanshoku decomp melds then
        yaku_lst := Score.Sanshoku :: !yaku_lst;
      if check_itsu decomp melds then yaku_lst := Score.Itsu :: !yaku_lst;
      if check_shousangen decomp melds then
        yaku_lst := Score.Shousangen :: !yaku_lst;

      if check_honroutou decomp melds then
        yaku_lst := Score.Honroutou :: !yaku_lst
      else if check_junchan decomp melds then
        yaku_lst := Score.Junchan :: !yaku_lst
      else if check_chanta decomp melds then
        yaku_lst := Score.Chanta :: !yaku_lst;

      if check_ryanpeiko decomp melds then (
        yaku_lst := List.filter (fun y -> y <> Score.Iipeiko) !yaku_lst;
        yaku_lst := Score.Ryanpeiko :: !yaku_lst);

      (match check_suits all_tiles with
      | Some y -> yaku_lst := y :: !yaku_lst
      | None -> ());
      yaku_lst := !yaku_lst @ count_dora all_tiles indicators;

      (* 简单符数处理：门前清自摸=20，其他=30 *)
      let fu = if is_menzen && is_tsumo then 20 else 30 in

      if !yaku_lst <> [] then
        possible_results :=
          {
            Score.han = List.length !yaku_lst;
            yaku_list = !yaku_lst;
            fu;
            points = 0;
          }
          :: !possible_results)
    partitions;

  (* 2. 七对子判定 *)
  if is_menzen && check_chiitoitsu_hand hand then (
    let yaku_lst = ref [ Score.Chiitoitsu ] in
    if is_tsumo then yaku_lst := Score.MenzenTsumo :: !yaku_lst;
    if check_tanyao all_tiles then yaku_lst := Score.Tanyao :: !yaku_lst;
    if List.for_all is_terminal_or_honor all_tiles then
      yaku_lst := Score.Honroutou :: !yaku_lst;
    (match check_suits all_tiles with
    | Some y -> yaku_lst := y :: !yaku_lst
    | None -> ());
    yaku_lst := !yaku_lst @ count_dora all_tiles indicators;

    possible_results :=
      {
        Score.han = List.length !yaku_lst;
        yaku_list = !yaku_lst;
        fu = 25;
        points = 0;
      }
      :: !possible_results);

  let calc_han_total yaku_list =
    List.fold_left
      (fun acc y ->
        acc
        +
        match y with
        | Score.MenzenTsumo | Score.Riichi | Score.Ippatsu | Score.Pinfu
        | Score.Tanyao | Score.Iipeiko | Score.Yakuhai _ | Score.Rinshan ->
            1
        | Score.Chiitoitsu | Score.Toitoi | Score.Sanankou | Score.Sankantsu
        | Score.SanshokuDoukou | Score.Honroutou | Score.Shousangen ->
            2
        | Score.Sanshoku -> if is_menzen then 2 else 1
        | Score.Itsu -> if is_menzen then 2 else 1
        | Score.Chanta -> if is_menzen then 2 else 1
        | Score.Honitsu -> if is_menzen then 3 else 2
        | Score.Junchan -> if is_menzen then 3 else 2
        | Score.Ryanpeiko -> 3
        | Score.Chinitsu -> if is_menzen then 6 else 5
        | Score.Dora n -> n)
      0 yaku_list
  in

  if !possible_results = [] then None
  else
    let best_yaku =
      List.sort
        (fun a b ->
          compare
            (calc_han_total b.Score.yaku_list)
            (calc_han_total a.Score.yaku_list))
        !possible_results
      |> List.hd
    in
    Some { best_yaku with han = calc_han_total best_yaku.yaku_list }
