(* lib/hand.ml *)

(* ========================================== *)
(* 基础类型与定义 *)
(* ========================================== *)

(* 副露类型定义 (移到这里以打破循环依赖) *)
type meld = 
  | Chi of Tile.t * Tile.t * Tile.t
  | Pon of Tile.t * Tile.t * Tile.t
  | Kan of Tile.t * Tile.t * Tile.t * Tile.t

type t = Tile.t list (* 手牌就是牌的列表 *)

(* 基础列表操作 *)
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

(* 将牌转换为 0-33 的整数索引，用于数组查找 *)
let tile_to_id = function
  | Tile.Numbered (Tile.Man, n) -> n - 1
  | Tile.Numbered (Tile.Pin, n) -> 9 + (n - 1)
  | Tile.Numbered (Tile.Sou, n) -> 18 + (n - 1)
  | Tile.Honor h ->
      27 + (match h with
        | Tile.East -> 0 | Tile.South -> 1 | Tile.West -> 2 | Tile.North -> 3
        | Tile.Red -> 4 | Tile.Green -> 5 | Tile.White -> 6)

(* 生成所有 34 种牌的样板 *)
let all_tile_types =
  let suits = [ Tile.Man; Tile.Pin; Tile.Sou ] in
  let nums = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  let honors = [ Tile.East; Tile.South; Tile.West; Tile.North; Tile.Red; Tile.Green; Tile.White; ] in
  let numbered = List.concat_map (fun s -> List.map (fun n -> Tile.Numbered (s, n)) nums) suits in
  numbered @ List.map (fun h -> Tile.Honor h) honors

(* 将手牌转换为频率表 (Count Array) *)
let to_frequency_table hand =
  let counts = Array.make 34 0 in
  List.iter (fun t -> let id = tile_to_id t in counts.(id) <- counts.(id) + 1) hand;
  counts

(* ========================================== *)
(* 核心算法：向听数与牌理 (Shanten & A* *) 
(* ========================================== *)

(* 贪心算法：计算剩余牌能组成多少个搭子 (Tatsu) *)
let count_tatsu_greedy counts =
  let c = Array.copy counts in
  let tatsu = ref 0 in
  (* 1. 先找两面/边张 *)
  for i = 0 to 26 do
    while i mod 9 < 8 && c.(i) > 0 && c.(i + 1) > 0 do incr tatsu; c.(i) <- c.(i) - 1; c.(i + 1) <- c.(i + 1) - 1 done;
    (* 2. 再找嵌张 *)
    while i mod 9 < 7 && c.(i) > 0 && c.(i + 2) > 0 do incr tatsu; c.(i) <- c.(i) - 1; c.(i + 2) <- c.(i + 2) - 1 done
  done;
  (* 3. 最后找对子 *)
  for i = 0 to 33 do while c.(i) >= 2 do incr tatsu; c.(i) <- c.(i) - 2 done done;
  !tatsu

(* DFS 搜索最大面子数 (旧算法，用于计算基础 Shanten) *)
let rec search_max_score counts depth current_m =
  (* ... (代码逻辑同前，为节省篇幅略去细节) ... *)
  if depth >= 34 then
    let t = count_tatsu_greedy counts in
    let effective_tatsu = min t (4 - current_m) in
    (current_m * 2) + effective_tatsu
  else if counts.(depth) = 0 then search_max_score counts (depth + 1) current_m
  else
    let best = ref (-1) in
    (* 尝试刻子 *)
    if counts.(depth) >= 3 then (
      counts.(depth) <- counts.(depth) - 3;
      best := max !best (search_max_score counts depth (current_m + 1));
      counts.(depth) <- counts.(depth) + 3);
    (* 尝试顺子 *)
    if depth < 27 && depth mod 9 < 7 then
      if counts.(depth + 1) > 0 && counts.(depth + 2) > 0 then (
        counts.(depth) <- counts.(depth) - 1; counts.(depth + 1) <- counts.(depth + 1) - 1; counts.(depth + 2) <- counts.(depth + 2) - 1;
        best := max !best (search_max_score counts depth (current_m + 1));
        counts.(depth) <- counts.(depth) + 1; counts.(depth + 1) <- counts.(depth + 1) + 1; counts.(depth + 2) <- counts.(depth + 2) + 1);
    best := max !best (search_max_score counts (depth + 1) current_m);
    !best

(* A* 搜索算法模块 *)
module AStar = struct
  type state = { counts: int array; melds: int; idx: int; f_score: int; }
  (* 启发式函数: 剩余牌 / 3 * 2 (乐观估计面子分) *)
  let heuristic counts = let sum = Array.fold_left ( + ) 0 counts in (sum / 3) * 2
  let calc_f melds counts = (melds * 2) + (heuristic counts)
  
  let push_state queue state =
    let rec insert = function
      | [] -> [state]
      | h :: t -> if state.f_score > h.f_score then state :: h :: t else h :: insert t
    in insert queue

  let search_max_score_astar initial_counts =
    let max_score_found = ref (-1) in
    let initial_h = heuristic initial_counts in
    let start_node = { counts = Array.copy initial_counts; melds = 0; idx = 0; f_score = initial_h } in
    let queue = ref [start_node] in
    
    (* A* 主循环 *)
    let rec loop () =
      match !queue with
      | [] -> !max_score_found
      | current :: rest ->
          queue := rest;
          if current.f_score + 2 < !max_score_found then loop () (* 剪枝 *)
          else if current.idx >= 34 then (
            (* 到底了，计算搭子 *)
            let tatsu = count_tatsu_greedy current.counts in
            let final_score = (current.melds * 2) + tatsu in
            let valid_score = if current.melds + tatsu > 4 then (current.melds * 2) + (4 - current.melds) else final_score in
            if valid_score > !max_score_found then max_score_found := valid_score;
            loop ()
          ) else if current.counts.(current.idx) = 0 then (
            (* 当前牌无，跳过 *)
            let next_node = { current with idx = current.idx + 1 } in
            queue := push_state !queue next_node;
            loop ()
          ) else (
            (* 尝试组刻子 *)
            if current.counts.(current.idx) >= 3 then (
              let next_c = Array.copy current.counts in
              next_c.(current.idx) <- next_c.(current.idx) - 3;
              let next_m = current.melds + 1 in
              let next_node = { counts = next_c; melds = next_m; idx = current.idx; f_score = calc_f next_m next_c } in
              queue := push_state !queue next_node
            );
            (* 尝试组顺子 *)
            let i = current.idx in
            if i < 27 && (i mod 9 < 7) then (
              if current.counts.(i) > 0 && current.counts.(i+1) > 0 && current.counts.(i+2) > 0 then (
                let next_c = Array.copy current.counts in
                next_c.(i) <- next_c.(i) - 1; next_c.(i+1) <- next_c.(i+1) - 1; next_c.(i+2) <- next_c.(i+2) - 1;
                let next_m = current.melds + 1 in
                let next_node = { counts = next_c; melds = next_m; idx = i; f_score = calc_f next_m next_c } in
                queue := push_state !queue next_node
              )
            );
            (* 放弃这张牌 *)
            let next_c = Array.copy current.counts in
            let next_node = { counts = next_c; melds = current.melds; idx = current.idx + 1; f_score = calc_f current.melds next_c } in
            queue := push_state !queue next_node;
            loop ()
          )
    in loop ()
end

(* 七对子向听数计算 *)
let calculate_chiitoitsu_shanten counts =
  let pairs = ref 0 in
  let kinds = ref 0 in
  for i = 0 to 33 do
    if counts.(i) > 0 then incr kinds;
    if counts.(i) >= 2 then incr pairs;
  done;
  let shanten = 6 - !pairs in
  (* 七对子必须有7种不同的牌 *)
  if !kinds < 7 then shanten + (7 - !kinds) else shanten

(* 综合向听数计算 (标准型 + 七对子) *)
let calculate_shanten_astar hand =
  let counts = to_frequency_table hand in
  let base_score_max = ref (-1) in
  (* 枚举雀头 *)
  for i = 0 to 33 do
    if counts.(i) >= 2 then (
      counts.(i) <- counts.(i) - 2;
      let score = AStar.search_max_score_astar counts in
      if score > !base_score_max then base_score_max := score;
      counts.(i) <- counts.(i) + 2)
  done;
  (* 无雀头情况 *)
  let score_no_pair = AStar.search_max_score_astar counts in
  let shanten_standard = 8 - !base_score_max - 1 in 
  let shanten_no_pair = 8 - score_no_pair in
  let min_standard = min shanten_standard shanten_no_pair in
  let shanten_7 = calculate_chiitoitsu_shanten counts in
  min min_standard shanten_7

(* 计算进张数 (Ukeire) *)
let calc_ukeire_astar hand_13 visible_counts =
  let current_shanten = calculate_shanten_astar hand_13 in
  let effective_count = ref 0 in
  List.iter
    (fun tile ->
      let temp_hand = tile :: hand_13 in
      let new_shanten = calculate_shanten_astar temp_hand in
      (* 只有当摸到这张牌能让向听数减少时，才算进张 *)
      if new_shanten < current_shanten then
        let id = tile_to_id tile in
        let seen = visible_counts.(id) in
        let possible = 4 - seen in
        if possible > 0 then effective_count := !effective_count + possible)
    all_tile_types;
  !effective_count

(* 获取 AI 切牌建议 *)
let get_recommendations_astar hand_14 visible_counts =
  let base_shanten = calculate_shanten_astar hand_14 in
  let unique_tiles = List.sort_uniq Tile.compare hand_14 in
  List.filter_map (fun tile ->
    match remove_first hand_14 tile with
    | None -> None
    | Some hand_13 ->
        let new_shanten = calculate_shanten_astar hand_13 in
        (* 只有切掉牌后不退步，才考虑 *)
        if new_shanten > base_shanten then None
        else
          let ukeire = calc_ukeire_astar hand_13 visible_counts in
          if ukeire > 0 then Some (tile, ukeire) else None)
    unique_tiles
  |> List.sort (fun (_, a) (_, b) -> compare b a)

(* 为兼容性保留的别名 *)
let calculate_efficiency = get_recommendations_astar
let calculate_shanten = calculate_shanten_astar
let possible_sets _ = []
let is_complete hand = calculate_shanten hand <= -1

(* ========================================== *)
(* 役种判定与计分 (Yaku & Scoring) *)
(* ========================================== *)

module Score = struct
  type yaku = 
    | MenzenTsumo | Riichi | Ippatsu
    | Pinfu | Tanyao | Iipeiko | Yakuhai of string | Rinshan
    | Sanshoku | Itsu | Chanta | Chiitoitsu | Toitoi | Sanankou | Sankantsu | SanshokuDoukou | Honroutou | Shousangen
    | Honitsu | Junchan | Ryanpeiko
    | Chinitsu
    | Dora of int
  
  type result = { han: int; yaku_list: yaku list; fu: int; points: int; }
end

(* 手牌拆解结果 *)
type decomposition = { sequences: Tile.t list list; triplets: Tile.t list list; pair: Tile.t list; }

let is_terminal_or_honor = function Tile.Honor _ -> true | Tile.Numbered(_, n) -> n = 1 || n = 9
let is_honor = function Tile.Honor _ -> true | _ -> false
let get_suit = function Tile.Numbered(s, _) -> Some s | Tile.Honor _ -> None

(* 1. 将手牌拆解为所有的 (面子+雀头) 组合 *)
let partition_hand (hand: t) : decomposition list =
  let counts = to_frequency_table hand in
  let results = ref [] in
  let rec solve c seqs trips pair_opt idx =
    if idx >= 34 then match pair_opt with Some p -> results := { sequences = seqs; triplets = trips; pair = p } :: !results | None -> ()
    else if c.(idx) == 0 then solve c seqs trips pair_opt (idx + 1)
    else (
      (* 尝试雀头 *)
      if pair_opt = None && c.(idx) >= 2 then (
        c.(idx) <- c.(idx) - 2; let tile = List.nth all_tile_types idx in
        solve c seqs trips (Some [tile; tile]) idx; c.(idx) <- c.(idx) + 2);
      (* 尝试刻子 *)
      if c.(idx) >= 3 then (
        c.(idx) <- c.(idx) - 3; let tile = List.nth all_tile_types idx in
        solve c seqs ([tile; tile; tile] :: trips) pair_opt idx; c.(idx) <- c.(idx) + 3);
      (* 尝试顺子 *)
      if idx < 27 && idx mod 9 < 7 && c.(idx) > 0 && c.(idx+1) > 0 && c.(idx+2) > 0 then (
        c.(idx) <- c.(idx) - 1; c.(idx+1) <- c.(idx+1) - 1; c.(idx+2) <- c.(idx+2) - 1;
        let t1 = List.nth all_tile_types idx in let t2 = List.nth all_tile_types (idx+1) in let t3 = List.nth all_tile_types (idx+2) in
        solve c ([t1;t2;t3] :: seqs) trips pair_opt idx;
        c.(idx) <- c.(idx) + 1; c.(idx+1) <- c.(idx+1) + 1; c.(idx+2) <- c.(idx+2) + 1)
    )
  in solve counts [] [] None 0; !results

let check_chiitoitsu_hand hand =
  let counts = to_frequency_table hand in
  let pairs = ref 0 in for i = 0 to 33 do if counts.(i) = 2 then incr pairs done; !pairs = 7

(* --- 役种判定 --- *)

let get_all_groups decomp melds =
  decomp.sequences @ decomp.triplets @ [decomp.pair] @
  (List.map (function Chi(a,b,c) -> [a;b;c] | Pon(a,_,_) -> [a;a;a] | Kan(a,_,_,_) -> [a;a;a;a]) melds)

(* 断幺九 *)
let check_tanyao all_tiles = not (List.exists is_terminal_or_honor all_tiles)

(* 役牌 *)
let check_yakuhai decomp melds round_wind seat_wind =
  let check_group tiles = match List.hd tiles with
    | Tile.Honor h ->
        let yaku = [] in
        let yaku = if h = Tile.Red || h = Tile.Green || h = Tile.White then Score.Yakuhai(Tile.to_string (Tile.Honor h)) :: yaku else yaku in
        let yaku = if h = round_wind then Score.Yakuhai("场风") :: yaku else yaku in
        let yaku = if h = seat_wind then Score.Yakuhai("自风") :: yaku else yaku in yaku
    | _ -> []
  in
  let trip_yaku = List.concat_map check_group decomp.triplets in
  let meld_yaku = List.concat_map (function
    | Pon(t,_,_) | Kan(t,_,_,_) -> check_group [t] 
    | _ -> []
  ) melds in
  List.sort_uniq compare (trip_yaku @ meld_yaku)

(* 平和 *)
let check_pinfu decomp melds round_wind seat_wind =
  if melds <> [] || decomp.triplets <> [] then false else
    match List.hd decomp.pair with Tile.Honor h -> not (h = Tile.Red || h = Tile.Green || h = Tile.White || h = round_wind || h = seat_wind) | _ -> true

(* 一盃口 / 二盃口 *)
let count_identical_seqs seqs =
  let sorted = List.sort compare seqs in
  let rec count acc = function a :: b :: rest -> if compare a b = 0 then count (acc + 1) rest else count acc (b :: rest) | _ -> acc in count 0 sorted
let check_iipeiko decomp melds = melds = [] && (count_identical_seqs decomp.sequences) = 1
let check_ryanpeiko decomp melds = melds = [] && (count_identical_seqs decomp.sequences) = 2

(* [修复] 三色同顺: 改用 List 遍历避免 Hashtbl 错误 *)
let check_sanshoku decomp melds =
  let all_seqs = decomp.sequences @ List.filter_map (function Chi(a,b,c) -> Some [a;b;c] | _ -> None) melds in
  let get_start_num seq = match List.sort Tile.compare seq with Tile.Numbered(_, n) :: _ -> Some n | _ -> None in
  let get_suit seq = match List.hd seq with Tile.Numbered(s,_) -> Some s | _ -> None in
  let nums = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  List.exists (fun n ->
    let seqs_with_n = List.filter (fun s -> get_start_num s = Some n) all_seqs in
    let suits = List.filter_map get_suit seqs_with_n in
    List.mem Tile.Man suits && List.mem Tile.Pin suits && List.mem Tile.Sou suits
  ) nums

(* [修复] 一气通贯: 移除 Hand. 前缀 *)
let check_itsu decomp melds =
  let all_seqs = decomp.sequences @ List.filter_map (function Chi(a,b,c) -> Some [a;b;c] | _ -> None) melds in
  let check_suit s_type = 
    let has n = List.exists (fun s -> match List.hd s with Tile.Numbered(st, num) -> st = s_type && num = n | _ -> false) all_seqs in 
    has 1 && has 4 && has 7 
  in check_suit Tile.Man || check_suit Tile.Pin || check_suit Tile.Sou

let check_toitoi decomp = decomp.sequences = []
let check_sanankou decomp melds is_tsumo = let hand_trips = List.length decomp.triplets in if is_tsumo then hand_trips >= 3 else hand_trips >= 3 
let check_sankantsu melds = let kans = List.filter (function Kan _ -> true | _ -> false) melds in List.length kans >= 3

(* [修复] 三色同刻: 改用 List 遍历避免 Hashtbl 错误 *)
let check_sanshoku_doukou decomp melds =
  let all_trips = decomp.triplets @ List.filter_map (function Pon(a,_,_) -> Some [a;a;a] | Kan(a,_,_,_) -> Some [a;a;a;a] | _ -> None) melds in
  let get_num t = match List.hd t with Tile.Numbered(_, n) -> Some n | _ -> None in
  let get_suit t = match List.hd t with Tile.Numbered(s,_) -> Some s | _ -> None in
  let nums = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  List.exists (fun n ->
    let trips_with_n = List.filter (fun t -> get_num t = Some n) all_trips in
    let suits = List.filter_map get_suit trips_with_n in
    List.mem Tile.Man suits && List.mem Tile.Pin suits && List.mem Tile.Sou suits
  ) nums

(* 小三元 *)
let check_shousangen decomp melds =
  let all_trips = decomp.triplets @ List.filter_map (function Pon(a,_,_) -> Some [a;a;a] | Kan(a,_,_,_) -> Some [a;a;a;a] | _ -> None) melds in
  let has_dragon h = List.exists (fun t -> match List.hd t with Tile.Honor x -> x = h | _ -> false) all_trips in
  let dragons = [Tile.White; Tile.Green; Tile.Red] in
  let triplet_dragons = List.filter has_dragon dragons in
  let pair_dragon = match List.hd decomp.pair with Tile.Honor h -> if List.mem h dragons then Some h else None | _ -> None in
  match pair_dragon with Some p -> List.length triplet_dragons >= 2 && not (List.mem p triplet_dragons) | None -> false

(* 全带系 & 混老头 *)
let check_junchan decomp melds = let groups = get_all_groups decomp melds in List.for_all (fun g -> List.exists (function Tile.Numbered(_, n) -> n = 1 || n = 9 | _ -> false) g) groups
let check_honroutou decomp melds = let groups = get_all_groups decomp melds in List.for_all (fun g -> List.for_all is_terminal_or_honor g) groups
let check_chanta decomp melds = let groups = get_all_groups decomp melds in List.for_all (fun g -> List.exists is_terminal_or_honor g) groups

(* 染手 (混一色/清一色) *)
let check_suits all_tiles =
  let suits = List.filter_map get_suit all_tiles in let has_honor = List.exists is_honor all_tiles in let uniq_suits = List.sort_uniq compare suits in
  match (List.length uniq_suits, has_honor) with (1, false) -> Some Score.Chinitsu | (1, true) -> Some Score.Honitsu | _ -> None

(* 宝牌计算 *)
let count_dora all_tiles indicators =
  let doras = List.map Tile.next_dora indicators in
  let count = List.fold_left (fun acc t -> let matches = List.filter (fun d -> Tile.compare t d = 0) doras in acc + List.length matches) 0 all_tiles in
  if count > 0 then [Score.Dora count] else []

(* 最终算分函数 *)
let calculate_score (hand: t) (melds: meld list) (indicators: Tile.t list) (round_wind: Tile.honor) (seat_wind: Tile.honor) (is_tsumo: bool) (is_rinshan: bool) : Score.result option =
  
  (* 构造所有手牌（含副露）*)
  let all_tiles = hand @ (List.flatten (List.map (function Chi(a,b,c) -> [a;b;c] | Pon(a,_,_) -> [a;a;a] | Kan(a,_,_,_) -> [a;a;a;a]) melds)) in
  let is_menzen = (melds = []) in
  let possible_results = ref [] in
  
  (* 路径 A: 标准拆解 (Standard Decomposition) *)
  let partitions = partition_hand hand in
  List.iter (fun decomp ->
    let yaku_lst = ref [] in
    
    (* 1番役 *)
    if is_menzen && is_tsumo then yaku_lst := Score.MenzenTsumo :: !yaku_lst;
    if is_rinshan then yaku_lst := Score.Rinshan :: !yaku_lst;
    if check_tanyao all_tiles then yaku_lst := Score.Tanyao :: !yaku_lst;
    if check_pinfu decomp melds round_wind seat_wind then yaku_lst := Score.Pinfu :: !yaku_lst;
    if check_iipeiko decomp melds then yaku_lst := Score.Iipeiko :: !yaku_lst;
    yaku_lst := !yaku_lst @ (check_yakuhai decomp melds round_wind seat_wind);
    
    (* 2番役 *)
    if check_toitoi decomp then yaku_lst := Score.Toitoi :: !yaku_lst;
    if check_sanankou decomp melds is_tsumo then yaku_lst := Score.Sanankou :: !yaku_lst;
    if check_sankantsu melds then yaku_lst := Score.Sankantsu :: !yaku_lst;
    if check_sanshoku_doukou decomp melds then yaku_lst := Score.SanshokuDoukou :: !yaku_lst;
    if check_sanshoku decomp melds then yaku_lst := Score.Sanshoku :: !yaku_lst;
    if check_itsu decomp melds then yaku_lst := Score.Itsu :: !yaku_lst;
    if check_shousangen decomp melds then yaku_lst := Score.Shousangen :: !yaku_lst;
    
    (* 复合役处理 *)
    if check_honroutou decomp melds then yaku_lst := Score.Honroutou :: !yaku_lst 
    else if check_junchan decomp melds then yaku_lst := Score.Junchan :: !yaku_lst 
    else if check_chanta decomp melds then yaku_lst := Score.Chanta :: !yaku_lst;
    
    if check_ryanpeiko decomp melds then (yaku_lst := List.filter (fun y -> y <> Score.Iipeiko) !yaku_lst; yaku_lst := Score.Ryanpeiko :: !yaku_lst);
    
    (* 染手 & 宝牌 *)
    (match check_suits all_tiles with Some y -> yaku_lst := y :: !yaku_lst | None -> ());
    yaku_lst := !yaku_lst @ (count_dora all_tiles indicators);
    
    possible_results := !yaku_lst :: !possible_results
  ) partitions;

  (* 路径 B: 七对子 (Chiitoitsu) *)
(* 路径 B: 七对子 (Chiitoitsu) *)
  if is_menzen && check_chiitoitsu_hand hand then (
    let yaku_lst = ref [Score.Chiitoitsu] in
    
    if is_tsumo then yaku_lst := Score.MenzenTsumo :: !yaku_lst;
    if check_tanyao all_tiles then yaku_lst := Score.Tanyao :: !yaku_lst;
    
    (* [修复] 混老头判定逻辑修正 *)
    (* 错误逻辑: check_honroutou {sequences=[];...} []  <- 这会返回 true 因为列表为空 *)
    (* 正确逻辑: 检查所有牌是否都是么九牌 *)
    if List.for_all is_terminal_or_honor all_tiles then yaku_lst := Score.Honroutou :: !yaku_lst; 
    
    (match check_suits all_tiles with Some y -> yaku_lst := y :: !yaku_lst | None -> ());
    yaku_lst := !yaku_lst @ (count_dora all_tiles indicators);
    
    possible_results := !yaku_lst :: !possible_results
  );

  (* 计算最高番数 *)
  let calc_han_total yaku_list = List.fold_left (fun acc y -> acc + match y with
    | Score.MenzenTsumo | Score.Riichi | Score.Ippatsu | Score.Pinfu | Score.Tanyao | Score.Iipeiko | Score.Yakuhai _ | Score.Rinshan -> 1
    | Score.Chiitoitsu | Score.Toitoi | Score.Sanankou | Score.Sankantsu | Score.SanshokuDoukou | Score.Honroutou | Score.Shousangen -> 2
    | Score.Sanshoku -> if is_menzen then 2 else 1
    | Score.Itsu -> if is_menzen then 2 else 1
    | Score.Chanta -> if is_menzen then 2 else 1
    | Score.Honitsu -> if is_menzen then 3 else 2
    | Score.Junchan -> if is_menzen then 3 else 2
    | Score.Ryanpeiko -> 3
    | Score.Chinitsu -> if is_menzen then 6 else 5
    | Score.Dora n -> n
  ) 0 yaku_list in

  (* 返回最佳结果 *)
  if !possible_results = [] then None else 
    let best_yaku = List.sort (fun a b -> compare (calc_han_total b) (calc_han_total a)) !possible_results |> List.hd in 
    Some { Score.han = calc_han_total best_yaku; yaku_list = best_yaku; fu = 30; points = 0 }