(* lib/hand.ml *)
open Core
type meld =
  | Chi of Tile.t * Tile.t * Tile.t
  | Pon of Tile.t * Tile.t * Tile.t
  | Kan of Tile.t * Tile.t * Tile.t * Tile.t
[@@deriving compare, sexp]

type t = Tile.t list [@@deriving compare, sexp]

let empty = []
let sort hand = List.sort hand ~compare:Tile.compare
let add hand tile = sort (tile :: hand)

let to_string hand =
  if List.is_empty hand then "(empty)"
  else hand |> List.map ~f:Tile.to_string |> String.concat ~sep:" "

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
  let nums = List.init 9 ~f:(fun i -> i + 1) in
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
    List.concat_map suits ~f:(fun s ->
        List.map nums ~f:(fun n -> Tile.Numbered (s, n)))
  in
  numbered @ List.map honors ~f:(fun h -> Tile.Honor h)

let to_frequency_table hand =
  let counts = Array.create ~len:34 0 in
  List.iter hand ~f:(fun t ->
      let id = tile_to_id t in
      counts.(id) <- counts.(id) + 1);
  counts

let is_terminal_or_honor = function
  | Tile.Honor _ -> true
  | Tile.Numbered (_, n) -> n = 1 || n = 9

let is_honor = function Tile.Honor _ -> true | _ -> false
let get_suit = function Tile.Numbered (s, _) -> Some s | Tile.Honor _ -> None

module DecompositionSearch = struct
  type t = {
    counts : int array;
    idx : int;
    remaining : int;
    melds : int;
    tatsu : int;
    has_head : bool;
  }

  let compare a b =
    let c = Int.compare a.idx b.idx in
    if c <> 0 then c
    else
      let c2 = Int.compare a.remaining b.remaining in
      if c2 <> 0 then c2
      else
        let c3 = Int.compare a.melds b.melds in
        if c3 <> 0 then c3
        else
          let c4 = Int.compare a.tatsu b.tatsu in
          if c4 <> 0 then c4
          else
            let c5 = Bool.compare a.has_head b.has_head in
            if c5 <> 0 then c5 else Array.compare Int.compare a.counts b.counts

  let heuristic state = -1.0 *. Float.of_int state.remaining
  let is_goal state = state.idx >= 34

  let neighbors state =
    if state.idx >= 34 then []
    else
      let c = state.counts.(state.idx) in

      if c = 0 then [ ({ state with idx = state.idx + 1 }, 0.0) ]
      else
        let moves = ref [] in

        let next_c_skip = Array.copy state.counts in
        next_c_skip.(state.idx) <- c - 1;
        moves :=
          ( { state with counts = next_c_skip; remaining = state.remaining - 1 },
            0.0 )
          :: !moves;
        if state.melds + state.tatsu < 4 then (
          if c >= 3 then (
            let next_c = Array.copy state.counts in
            next_c.(state.idx) <- c - 3;
            moves :=
              ( {
                  state with
                  counts = next_c;
                  remaining = state.remaining - 3;
                  melds = state.melds + 1;
                },
                -2.0 )
              :: !moves);
          if
            state.idx < 27
            && state.idx % 9 < 7
            && state.counts.(state.idx + 1) > 0
            && state.counts.(state.idx + 2) > 0
          then (
            let next_c = Array.copy state.counts in
            next_c.(state.idx) <- c - 1;
            next_c.(state.idx + 1) <- next_c.(state.idx + 1) - 1;
            next_c.(state.idx + 2) <- next_c.(state.idx + 2) - 1;
            moves :=
              ( {
                  state with
                  counts = next_c;
                  remaining = state.remaining - 3;
                  melds = state.melds + 1;
                },
                -2.0 )
              :: !moves));
        if (not state.has_head) && c >= 2 then (
          let next_c = Array.copy state.counts in
          next_c.(state.idx) <- c - 2;
          moves :=
            ( {
                state with
                counts = next_c;
                remaining = state.remaining - 2;
                has_head = true;
              },
              -1.0 )
            :: !moves);

        if state.melds + state.tatsu < 4 then (
          if c >= 2 then (
            let next_c = Array.copy state.counts in
            next_c.(state.idx) <- c - 2;
            moves :=
              ( {
                  state with
                  counts = next_c;
                  remaining = state.remaining - 2;
                  tatsu = state.tatsu + 1;
                },
                -1.0 )
              :: !moves);
          if
            state.idx < 27
            && state.idx % 9 < 8
            && state.counts.(state.idx + 1) > 0
          then (
            let next_c = Array.copy state.counts in
            next_c.(state.idx) <- c - 1;
            next_c.(state.idx + 1) <- next_c.(state.idx + 1) - 1;
            moves :=
              ( {
                  state with
                  counts = next_c;
                  remaining = state.remaining - 2;
                  tatsu = state.tatsu + 1;
                },
                -1.0 )
              :: !moves);
          if
            state.idx < 27
            && state.idx % 9 < 7
            && state.counts.(state.idx + 2) > 0
          then (
            let next_c = Array.copy state.counts in
            next_c.(state.idx) <- c - 1;
            next_c.(state.idx + 2) <- next_c.(state.idx + 2) - 1;
            moves :=
              ( {
                  state with
                  counts = next_c;
                  remaining = state.remaining - 2;
                  tatsu = state.tatsu + 1;
                },
                -1.0 )
              :: !moves));

        !moves
end

module Solver = Astar.Make (DecompositionSearch)

let calculate_standard_shanten counts =
  let total_tiles = Array.fold counts ~init:0 ~f:( + ) in
  let start_node =
    {
      DecompositionSearch.counts;
      idx = 0;
      remaining = total_tiles;
      melds = 0;
      tatsu = 0;
      has_head = false;
    }
  in
  let start_g = 8.0 in

  match Solver.search start_node with
  | None -> 8
  | Some (cost_change, _) ->
      Float.to_int (start_g +. cost_change)

let calculate_chiitoitsu_shanten counts =
  let pairs = ref 0 in
  let kinds = ref 0 in
  for i = 0 to 33 do
    if counts.(i) > 0 then incr kinds;
    if counts.(i) >= 2 then incr pairs
  done;
  let shanten = 6 - !pairs in
  if !kinds < 7 then shanten + (7 - !kinds) else shanten

let calculate_shanten hand =
  let counts = to_frequency_table hand in
  Int.min
    (calculate_standard_shanten (Array.copy counts))
    (calculate_chiitoitsu_shanten counts)

let is_complete hand = calculate_shanten hand <= -1
let possible_sets _ = []


let calc_ukeire hand_13 visible_counts =
  let current_shanten = calculate_shanten hand_13 in
  let effective_count = ref 0 in
  List.iter all_tile_types ~f:(fun tile ->
      let temp_hand = tile :: hand_13 in
      if calculate_shanten temp_hand < current_shanten then
        let id = tile_to_id tile in
        let seen = visible_counts.(id) in
        let possible = 4 - seen in
        if possible > 0 then effective_count := !effective_count + possible);
  !effective_count

let get_recommendations_astar hand visible_counts =
  let base_shanten = calculate_shanten hand in
  let unique_tiles = List.dedup_and_sort hand ~compare:Tile.compare in
  List.filter_map unique_tiles ~f:(fun tile ->
      match remove_first hand tile with
      | None -> None
      | Some hand_13 ->
          let new_shanten = calculate_shanten hand_13 in
          if new_shanten > base_shanten then None
          else
            let ukeire = calc_ukeire hand_13 visible_counts in
            if ukeire > 0 then Some (tile, ukeire) else None)
  |> List.sort ~compare:(fun (t1, u1) (t2, u2) ->
         let c = Int.compare u2 u1 in
         if c <> 0 then c else Tile.compare t2 t1)

let calculate_efficiency = get_recommendations_astar


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
  [@@deriving compare, sexp]

  type result = { han : int; yaku_list : yaku list; fu : int; points : int }
  [@@deriving sexp]
end

type decomposition = {
  sequences : Tile.t list list;
  triplets : Tile.t list list;
  pair : Tile.t list;
}

let partition_hand (hand : t) : decomposition list =
  let counts = to_frequency_table hand in
  let results = ref [] in

  let rec solve idx seqs trips pair_opt =
    if idx >= 34 then
      match pair_opt with
      | Some p ->
          results :=
            { sequences = seqs; triplets = trips; pair = p } :: !results
      | None -> ()
    else if counts.(idx) = 0 then solve (idx + 1) seqs trips pair_opt
    else (
      if counts.(idx) >= 2 && Option.is_none pair_opt then (
        counts.(idx) <- counts.(idx) - 2;
        let tile = List.nth_exn all_tile_types idx in
        solve idx seqs trips (Some [ tile; tile ]);
        counts.(idx) <- counts.(idx) + 2 (* Backtrack *));

      if counts.(idx) >= 3 then (
        counts.(idx) <- counts.(idx) - 3;
        let tile = List.nth_exn all_tile_types idx in
        solve idx seqs ([ tile; tile; tile ] :: trips) pair_opt;
        counts.(idx) <- counts.(idx) + 3 (* Backtrack *));

      if
        idx < 27
        && idx % 9 < 7
        && counts.(idx) > 0
        && counts.(idx + 1) > 0
        && counts.(idx + 2) > 0
      then (
        counts.(idx) <- counts.(idx) - 1;
        counts.(idx + 1) <- counts.(idx + 1) - 1;
        counts.(idx + 2) <- counts.(idx + 2) - 1;

        let t1 = List.nth_exn all_tile_types idx in
        let t2 = List.nth_exn all_tile_types (idx + 1) in
        let t3 = List.nth_exn all_tile_types (idx + 2) in

        solve idx ([ t1; t2; t3 ] :: seqs) trips pair_opt;

        (* Backtrack *)
        counts.(idx) <- counts.(idx) + 1;
        counts.(idx + 1) <- counts.(idx + 1) + 1;
        counts.(idx + 2) <- counts.(idx + 2) + 1))
  in
  solve 0 [] [] None;
  !results

let get_all_groups decomp melds =
  decomp.sequences @ decomp.triplets @ [ decomp.pair ]
  @ List.map melds ~f:(function
      | Chi (a, b, c) -> [ a; b; c ]
      | Pon (a, _, _) -> [ a; a; a ]
      | Kan (a, _, _, _) -> [ a; a; a; a ])

let check_tanyao all_tiles = not (List.exists all_tiles ~f:is_terminal_or_honor)

let check_yakuhai decomp melds round_wind seat_wind =
  let check_group tiles =
    match List.hd tiles with
    | Some (Tile.Honor h) ->
        let yaku = [] in
        let yaku =
          if
            Tile.equal_honor h Tile.Red
            || Tile.equal_honor h Tile.Green
            || Tile.equal_honor h Tile.White
          then Score.Yakuhai (Tile.to_string (Tile.Honor h)) :: yaku
          else yaku
        in
        let yaku =
          if Tile.equal_honor h round_wind then
            Score.Yakuhai "Round Wind" :: yaku
          else yaku
        in
        let yaku =
          if Tile.equal_honor h seat_wind then Score.Yakuhai "Seat Wind" :: yaku
          else yaku
        in
        yaku
    | _ -> []
  in
  let trip_yaku = List.concat_map decomp.triplets ~f:check_group in
  let meld_yaku =
    List.concat_map melds ~f:(function
      | Pon (t, _, _) | Kan (t, _, _, _) -> check_group [ t ]
      | _ -> [])
  in
  List.dedup_and_sort (trip_yaku @ meld_yaku) ~compare:Score.compare_yaku

let check_pinfu decomp melds round_wind seat_wind =
  if (not (List.is_empty melds)) || not (List.is_empty decomp.triplets) then
    false
  else
    match List.hd decomp.pair with
    | Some (Tile.Honor h) ->
        not
          (Tile.equal_honor h Tile.Red
          || Tile.equal_honor h Tile.Green
          || Tile.equal_honor h Tile.White
          || Tile.equal_honor h round_wind
          || Tile.equal_honor h seat_wind)
    | _ -> true

let count_identical_seqs seqs =
  let sorted = List.sort seqs ~compare:(List.compare Tile.compare) in
  let rec count acc = function
    | a :: b :: rest ->
        if List.compare Tile.compare a b = 0 then count (acc + 1) rest
        else count acc (b :: rest)
    | _ -> acc
  in
  count 0 sorted

let check_iipeiko decomp melds =
  List.is_empty melds && count_identical_seqs decomp.sequences = 1

let check_ryanpeiko decomp melds =
  List.is_empty melds && count_identical_seqs decomp.sequences = 2

let check_sanshoku decomp melds =
  let all_seqs =
    decomp.sequences
    @ List.filter_map melds ~f:(function
        | Chi (a, b, c) -> Some [ a; b; c ]
        | _ -> None)
  in
  let get_start_num seq =
    match List.sort seq ~compare:Tile.compare with
    | Tile.Numbered (_, n) :: _ -> Some n
    | _ -> None
  in
  let get_suit seq =
    match List.hd_exn seq with Tile.Numbered (s, _) -> Some s | _ -> None
  in
  let nums = List.init 9 ~f:(fun i -> i + 1) in
  List.exists nums ~f:(fun n ->
      let seqs_with_n =
        List.filter all_seqs ~f:(fun s ->
            match get_start_num s with Some sn -> sn = n | None -> false)
      in
      let suits = List.filter_map seqs_with_n ~f:get_suit in
      List.mem suits Tile.Man ~equal:Tile.equal_suit
      && List.mem suits Tile.Pin ~equal:Tile.equal_suit
      && List.mem suits Tile.Sou ~equal:Tile.equal_suit)

let check_itsu decomp melds =
  let all_seqs =
    decomp.sequences
    @ List.filter_map melds ~f:(function
        | Chi (a, b, c) -> Some [ a; b; c ]
        | _ -> None)
  in
  let check_suit s_type =
    let has n =
      List.exists all_seqs ~f:(fun s ->
          match List.hd_exn s with
          | Tile.Numbered (st, num) -> Tile.equal_suit st s_type && num = n
          | _ -> false)
    in
    has 1 && has 4 && has 7
  in
  check_suit Tile.Man || check_suit Tile.Pin || check_suit Tile.Sou

let check_toitoi decomp = List.is_empty decomp.sequences

let check_sanankou decomp melds is_tsumo =
  let hand_trips = List.length decomp.triplets in
  if is_tsumo then hand_trips >= 3 else hand_trips >= 3

let check_sankantsu melds =
  let kans = List.filter melds ~f:(function Kan _ -> true | _ -> false) in
  List.length kans >= 3

let check_sanshoku_doukou decomp melds =
  let all_trips =
    decomp.triplets
    @ List.filter_map melds ~f:(function
        | Pon (a, _, _) -> Some [ a; a; a ]
        | Kan (a, _, _, _) -> Some [ a; a; a; a ]
        | _ -> None)
  in
  let get_num t =
    match List.hd_exn t with Tile.Numbered (_, n) -> Some n | _ -> None
  in
  let get_suit t =
    match List.hd_exn t with Tile.Numbered (s, _) -> Some s | _ -> None
  in
  let nums = List.init 9 ~f:(fun i -> i + 1) in
  List.exists nums ~f:(fun n ->
      let trips_with_n =
        List.filter all_trips ~f:(fun t ->
            match get_num t with Some tn -> tn = n | None -> false)
      in
      let suits = List.filter_map trips_with_n ~f:get_suit in
      List.mem suits Tile.Man ~equal:Tile.equal_suit
      && List.mem suits Tile.Pin ~equal:Tile.equal_suit
      && List.mem suits Tile.Sou ~equal:Tile.equal_suit)

let check_shousangen decomp melds =
  let all_trips =
    decomp.triplets
    @ List.filter_map melds ~f:(function
        | Pon (a, _, _) -> Some [ a; a; a ]
        | Kan (a, _, _, _) -> Some [ a; a; a; a ]
        | _ -> None)
  in
  let has_dragon h =
    List.exists all_trips ~f:(fun t ->
        match List.hd_exn t with
        | Tile.Honor x -> Tile.equal_honor x h
        | _ -> false)
  in
  let dragons = [ Tile.White; Tile.Green; Tile.Red ] in
  let triplet_dragons = List.filter dragons ~f:has_dragon in
  let pair_dragon =
    match List.hd_exn decomp.pair with
    | Tile.Honor h ->
        if List.mem dragons h ~equal:Tile.equal_honor then Some h else None
    | _ -> None
  in
  match pair_dragon with
  | Some p ->
      List.length triplet_dragons >= 2
      && not (List.mem triplet_dragons p ~equal:Tile.equal_honor)
  | None -> false

let check_junchan decomp melds =
  let groups = get_all_groups decomp melds in
  List.for_all groups ~f:(fun g ->
      List.exists g ~f:(function
        | Tile.Numbered (_, n) -> n = 1 || n = 9
        | _ -> false))

let check_honroutou decomp melds =
  let groups = get_all_groups decomp melds in
  List.for_all groups ~f:(fun g -> List.for_all g ~f:is_terminal_or_honor)

let check_chanta decomp melds =
  let groups = get_all_groups decomp melds in
  List.for_all groups ~f:(fun g -> List.exists g ~f:is_terminal_or_honor)

let check_suits all_tiles =
  let suits = List.filter_map all_tiles ~f:get_suit in
  let has_honor = List.exists all_tiles ~f:is_honor in
  let uniq_suits = List.dedup_and_sort suits ~compare:Tile.compare_suit in
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
  let doras = List.map indicators ~f:Tile.next_dora in
  let count =
    List.fold all_tiles ~init:0 ~f:(fun acc t ->
        let matches = List.filter doras ~f:(fun d -> Tile.compare t d = 0) in
        acc + List.length matches)
  in
  if count > 0 then [ Score.Dora count ] else []

let calculate_score (hand : t) (melds : meld list) (indicators : Tile.t list)
    (round_wind : Tile.honor) (seat_wind : Tile.honor) (is_tsumo : bool)
    (is_rinshan : bool) : Score.result option =
  let all_tiles =
    hand
    @ List.concat_map melds ~f:(function
        | Chi (a, b, c) -> [ a; b; c ]
        | Pon (a, _, _) -> [ a; a; a ]
        | Kan (a, _, _, _) -> [ a; a; a; a ])
  in
  let is_menzen = List.is_empty melds in
  let possible_results = ref [] in
  let partitions = partition_hand hand in
  List.iter partitions ~f:(fun decomp ->
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
        yaku_lst :=
          List.filter !yaku_lst ~f:(fun y -> Poly.( <> ) y Score.Iipeiko);
        yaku_lst := Score.Ryanpeiko :: !yaku_lst);
      (match check_suits all_tiles with
      | Some y -> yaku_lst := y :: !yaku_lst
      | None -> ());
      yaku_lst := !yaku_lst @ count_dora all_tiles indicators;
      let fu = if is_menzen && is_tsumo then 20 else 30 in
      if not (List.is_empty !yaku_lst) then
        possible_results :=
          {
            Score.han = List.length !yaku_lst;
            yaku_list = !yaku_lst;
            fu;
            points = 0;
          }
          :: !possible_results);

  if is_menzen && check_chiitoitsu_hand hand then (
    let yaku_lst = ref [ Score.Chiitoitsu ] in
    if is_tsumo then yaku_lst := Score.MenzenTsumo :: !yaku_lst;
    if check_tanyao all_tiles then yaku_lst := Score.Tanyao :: !yaku_lst;
    if List.for_all all_tiles ~f:is_terminal_or_honor then
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
    List.fold yaku_list ~init:0 ~f:(fun acc y ->
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
  in
  if List.is_empty !possible_results then None
  else
    let best_yaku =
      List.sort !possible_results ~compare:(fun a b ->
          Int.compare
            (calc_han_total b.Score.yaku_list)
            (calc_han_total a.Score.yaku_list))
      |> List.hd_exn
    in
    Some { best_yaku with han = calc_han_total best_yaku.yaku_list }
