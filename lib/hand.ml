(*hand.ml*)

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

let max_melds_ref = ref 4

let tile_to_id = function
  | Tile.Numbered (Tile.Man, n) -> n - 1
  | Tile.Numbered (Tile.Pin, n) -> 9 + (n - 1)
  | Tile.Numbered (Tile.Sou, n) -> 18 + (n - 1)
  | Tile.Honor h ->
      27
      + (match h with
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

let rec search_max_score counts depth current_m =
  if depth >= 34 then
    let t = count_tatsu_greedy counts in
    let effective_tatsu = min t (!max_melds_ref - current_m) in
    (current_m * 2) + effective_tatsu
  else if counts.(depth) = 0 then search_max_score counts (depth + 1) current_m
  else
    let best = ref (-1) in
    if counts.(depth) >= 3 then (
      counts.(depth) <- counts.(depth) - 3;
      best := max !best (search_max_score counts depth (current_m + 1));
      counts.(depth) <- counts.(depth) + 3);
    if depth < 27 && depth mod 9 < 7 then
      if counts.(depth + 1) > 0 && counts.(depth + 2) > 0 then (
        counts.(depth) <- counts.(depth) - 1;
        counts.(depth + 1) <- counts.(depth + 1) - 1;
        counts.(depth + 2) <- counts.(depth + 2) - 1;
        best := max !best (search_max_score counts depth (current_m + 1));
        counts.(depth) <- counts.(depth) + 1;
        counts.(depth + 1) <- counts.(depth + 1) + 1;
        counts.(depth + 2) <- counts.(depth + 2) + 1);
    best := max !best (search_max_score counts (depth + 1) current_m);
    !best

let calculate_shanten hand =
  let hand_len = List.length hand in
  max_melds_ref := hand_len / 3;
  let base = !max_melds_ref * 2 in
  let counts = to_frequency_table hand in
  let min_shanten = ref 8 in
  for i = 0 to 33 do
    if counts.(i) >= 2 then (
      counts.(i) <- counts.(i) - 2;
      let score = search_max_score counts 0 0 in
      let shanten = base - score - 1 in
      min_shanten := min !min_shanten shanten;
      counts.(i) <- counts.(i) + 2)
  done;
  let score_no_pair = search_max_score counts 0 0 in
  min_shanten := min !min_shanten (base - score_no_pair);
  !min_shanten

let calc_ukeire_count hand_13 visible_counts =
  let current_shanten = calculate_shanten hand_13 in
  let effective_count = ref 0 in
  List.iter
    (fun tile ->
      let temp_hand = tile :: hand_13 in
      let new_shanten = calculate_shanten temp_hand in
      if new_shanten < current_shanten then
        let id = tile_to_id tile in
        (* 修正逻辑：使用传入的 visible_counts *)
        let seen = visible_counts.(id) in
        let possible = 4 - seen in
        if possible > 0 then effective_count := !effective_count + possible)
    all_tile_types;
  !effective_count

let calculate_efficiency hand_14 visible_counts =
  let base_shanten = calculate_shanten hand_14 in
  let unique_tiles = List.sort_uniq Tile.compare hand_14 in
  let results =
    List.filter_map
      (fun tile ->
        match remove_first hand_14 tile with
        | None -> None
        | Some hand_13 ->
            let sh13 = calculate_shanten hand_13 in
            if sh13 > base_shanten then
              None
            else
              (* 传递 visible_counts *)
              let count = calc_ukeire_count hand_13 visible_counts in
              Some (tile, count))
      unique_tiles
  in
  List.sort (fun (_, a) (_, b) -> compare b a) results

let is_complete hand = calculate_shanten hand <= -1
let possible_sets _ = []

(* ========================================== *)
(* A* Algorithm Implementation Section        *)
(* ========================================== *)

module AStar = struct
  (* 定义搜索状态 *)
  type state = {
    counts: int array;  (* 剩余牌的频率表 *)
    melds: int;         (* 已经提取的面子数量 *)
    idx: int;           (* 当前处理到的牌索引 (0-33) *)
    f_score: int;       (* 优先级评分: g + h *)
  }

  (* 启发式函数: 剩余牌最多还能组多少个面子 (权重为2) *)
  (* 这是一个 Admissible Heuristic，因为它永远不会高估实际能组的面子数 *)
  let heuristic counts =
    let sum = Array.fold_left ( + ) 0 counts in
    (sum / 3) * 2

  (* 计算状态的优先级分数 *)
  let calc_f melds counts = 
    (melds * 2) + (heuristic counts)

  (* 简单的优先队列实现 (基于List排序，用于演示A*逻辑) *)
  (* 实际生产环境可用 Pairing Heap 或 Binary Heap 优化 *)
  let push_state queue state =
    let rec insert = function
      | [] -> [state]
      | h :: t -> 
          if state.f_score > h.f_score then state :: h :: t
          else h :: insert t
    in
    insert queue

  (* A* 搜索主逻辑: 寻找标准形的最大分值 (面子*2 + 搭子) *)
  let search_max_score_astar initial_counts =
    let max_score_found = ref (-1) in
    
    (* 初始状态 *)
    let initial_h = heuristic initial_counts in
    let start_node = { 
      counts = Array.copy initial_counts; 
      melds = 0; 
      idx = 0; 
      f_score = initial_h 
    } in
    
    let queue = ref [start_node] in

    let rec loop () =
      match !queue with
      | [] -> !max_score_found
      | current :: rest ->
          queue := rest;

          (* 剪枝: 如果当前节点的理论最大潜力都不如已知最优解，直接丢弃 *)
          (* 8 是假设的一手牌最大结构分 (4面子=8分) *)
          (* 这里放松限制，确保能搜索到底 *)
          if current.f_score + 2 < !max_score_found then loop ()
          else if current.idx >= 34 then (
            (* 到达搜索树底部，计算剩余搭子 *)
            let tatsu = count_tatsu_greedy current.counts in
            let final_score = (current.melds * 2) + tatsu in
            
            (* 限制: 面子+搭子不能超过4个 (标准型限制) *)
            let valid_score = 
              if current.melds + tatsu > 4 then (current.melds * 2) + (4 - current.melds)
              else final_score 
            in
            
            if valid_score > !max_score_found then max_score_found := valid_score;
            loop ()
          ) 
          else if current.counts.(current.idx) = 0 then (
            (* 当前牌没有了，跳到下一张 *)
            let next_node = { current with idx = current.idx + 1 } in
            (* 此时 f_score 不变或减小(因为h变了)，重新入队 *)
            (* 为了效率，这里可以直接递归而不入队，但为了保持A*结构我们入队 *)
            queue := push_state !queue next_node;
            loop ()
          ) 
          else (
            (* 扩展节点: 尝试三种操作 *)
            
            (* 1. 组刻子 (Triplet) *)
            if current.counts.(current.idx) >= 3 then (
              let next_c = Array.copy current.counts in
              next_c.(current.idx) <- next_c.(current.idx) - 3;
              let next_m = current.melds + 1 in
              let next_node = { 
                counts = next_c; 
                melds = next_m; 
                idx = current.idx; (* 索引不变，可能还有剩余做顺子 *)
                f_score = calc_f next_m next_c 
              } in
              queue := push_state !queue next_node
            );

            (* 2. 组顺子 (Sequence) - 仅限数牌 *)
            let i = current.idx in
            if i < 27 && (i mod 9 < 7) then (
              if current.counts.(i) > 0 && current.counts.(i+1) > 0 && current.counts.(i+2) > 0 then (
                let next_c = Array.copy current.counts in
                next_c.(i) <- next_c.(i) - 1;
                next_c.(i+1) <- next_c.(i+1) - 1;
                next_c.(i+2) <- next_c.(i+2) - 1;
                let next_m = current.melds + 1 in
                let next_node = { 
                  counts = next_c; 
                  melds = next_m; 
                  idx = i; (* 索引不变，可能还能组顺子 *)
                  f_score = calc_f next_m next_c 
                } in
                queue := push_state !queue next_node
              )
            );

            (* 3. 跳过 (Skip) - 把这张牌当做搭子或废牌留给最后处理 *)
            let next_c = Array.copy current.counts in
            (* 不做任何扣减，只移动索引 *)
            (* 注意：为了避免无限循环，这里不仅移动索引，通常意味着这张牌"不再作为面子的首张" *)
            let next_node = { 
              counts = next_c; 
              melds = current.melds; 
              idx = current.idx + 1;
              f_score = calc_f current.melds next_c
            } in
            queue := push_state !queue next_node;
            
            loop ()
          )
    in
    loop ()
end

(* 七对子判定 (Seven Pairs Check) *)
let calculate_chiitoitsu_shanten counts =
  let pairs = ref 0 in
  let kinds = ref 0 in
  for i = 0 to 33 do
    if counts.(i) > 0 then incr kinds;
    if counts.(i) >= 2 then incr pairs;
  done;
  (* 7对子向听 = 6 - 对子数 + (如果缺种类则补罚) *)
  (* 实际上标准公式: 6 - pairs + max(0, 7 - kinds) *)
  (* 但简单来说就是 6 - pairs，且手牌必须有7种以上不同的牌 *)
  let shanten = 6 - !pairs in
  if !kinds < 7 then shanten + (7 - !kinds) else shanten

(* A* 版向听数计算总入口 *)
let calculate_shanten_astar hand =
  let counts = to_frequency_table hand in
  let base_score_max = ref (-1) in
  
  (* 1. 标准形计算 (4面子1雀头) *)
  (* 策略：枚举雀头，然后用 A* 跑剩下的牌 *)
  for i = 0 to 33 do
    if counts.(i) >= 2 then (
      counts.(i) <- counts.(i) - 2;
      (* 调用 A* 核心 *)
      let score = AStar.search_max_score_astar counts in
      if score > !base_score_max then base_score_max := score;
      counts.(i) <- counts.(i) + 2 (* 回溯 *)
    )
  done;
  
  (* 同时也计算无雀头的情况 (单吊将) *)
  let score_no_pair = AStar.search_max_score_astar counts in
  (* 无雀头算出来的 score 是 面子*2 + 搭子 *)
  (* 实际上标准向听公式: 8 - 2*M - T *)
  (* 这里的 score 等价于 2*M + T *)
  
  (* 转换分数为向听数 *)
  (* 有雀头的情况: 向听 = 8 - (score + 1_pair) = 7 - score *)
  let shanten_standard = 8 - !base_score_max - 1 in 
  
  (* 无雀头的情况: 向听 = 8 - score *)
  let shanten_no_pair = 8 - score_no_pair in
  
  let min_standard = min shanten_standard shanten_no_pair in
  
  (* 2. 七对子计算 *)
  let shanten_7 = calculate_chiitoitsu_shanten counts in
  
  (* 3. 国士无双 (略，为了代码长度暂不写，通常很少用到) *)
  
  min min_standard shanten_7

(* A* 版进张数计算 (Helper) *)
let calc_ukeire_astar hand_13 visible_counts =
  let current_shanten = calculate_shanten_astar hand_13 in
  let effective_count = ref 0 in
  List.iter
    (fun tile ->
      let temp_hand = tile :: hand_13 in
      let new_shanten = calculate_shanten_astar temp_hand in
      if new_shanten < current_shanten then (
        (* 关键修改：从总数 4 中减去全场可见的牌数 *)
        let id = tile_to_id tile in
        let seen = visible_counts.(id) in
        let possible = 4 - seen in
        
        (* 如果 possible < 0，说明数据记错了，归零处理 *)
        let real_possible = max 0 possible in
        effective_count := !effective_count + real_possible
      )
    )
    all_tile_types;
  !effective_count

(* ========================================== *)
(* API: 使用 A* 的推荐函数                     *)
(* ========================================== *)
let get_recommendations_astar (hand_14: t) (visible_counts: int array) : (Tile.t * int) list =
  let base_shanten = calculate_shanten_astar hand_14 in
  let unique_tiles = List.sort_uniq Tile.compare hand_14 in
  
  let results =
    List.filter_map
      (fun tile ->
        match remove_first hand_14 tile with
        | None -> None
        | Some hand_13 ->
            let new_shanten = calculate_shanten_astar hand_13 in
            if new_shanten > base_shanten then 
              None 
            else
              (* 传入 visible_counts *)
              let ukeire = calc_ukeire_astar hand_13 visible_counts in
              if ukeire > 0 then Some (tile, ukeire) else None)
      unique_tiles
  in
  List.sort (fun (_, a) (_, b) -> compare b a) results
