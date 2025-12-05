(* lib/hand.ml *)
type t = Tile.t list

let empty = []
let sort = List.sort Tile.compare
let add hand tile = sort (tile :: hand)
let remove_first hand tile =
  let rec aux acc = function
    | [] -> None
    | h :: t -> if Tile.compare h tile = 0 then Some (List.rev acc @ t) else aux (h :: acc) t
  in aux [] hand
let to_string hand = hand |> List.map Tile.to_string |> String.concat " "

(* ========================================================== *)
(* 核心算法：标准麻将向听数 (Shanten) 计算                     *)
(* ========================================================== *)

(* 1. 基础映射 (0-33) *)
let tile_to_id = function
  | Tile.Numbered (Tile.Man, n) -> n - 1
  | Tile.Numbered (Tile.Pin, n) -> 9 + (n - 1)
  | Tile.Numbered (Tile.Sou, n) -> 18 + (n - 1)
  | Tile.Honor h -> 
      27 + (match h with
            | Tile.East -> 0 | Tile.South -> 1 | Tile.West -> 2 | Tile.North -> 3
            | Tile.Red -> 4 | Tile.Green -> 5 | Tile.White -> 6)

let all_tile_types = 
  let suits = [Tile.Man; Tile.Pin; Tile.Sou] in
  let nums = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let honors = [Tile.East; Tile.South; Tile.West; Tile.North; Tile.Red; Tile.Green; Tile.White] in
  let numbered = List.concat_map (fun s -> List.map (fun n -> Tile.Numbered (s, n)) nums) suits in
  let honor_tiles = List.map (fun h -> Tile.Honor h) honors in
  numbered @ honor_tiles

(* [关键] 每次调用都生成新数组，防止副作用污染 *)
let to_frequency_table (hand: t) : int array =
  let counts = Array.make 34 0 in
  List.iter (fun t -> let id = tile_to_id t in counts.(id) <- counts.(id) + 1) hand;
  counts

(* 2. 深度优先搜索 (DFS) 提取面子和搭子 *)
(* counts: 剩余牌的计数器 (会被修改并回溯)
   depth: 当前遍历的牌索引 (0-33)
   m: 已提取的面子数 (Meld)
   t: 已提取的搭子数 (Tatsu)
   max_score_ref: 记录 (2*M + T) 的最大值
*)
let rec search_groups counts depth m t max_score_ref =
  if depth >= 34 then (
    (* 遍历结束，计算分数 *)
    (* 规则：面子+搭子 最多 4 组 *)
    let valid_t = min t (4 - m) in 
    let current_score = (2 * m) + valid_t in
    if current_score > !max_score_ref then max_score_ref := current_score
  ) else (
    (* 剪枝：如果当前牌没了，直接看下一张 *)
    if counts.(depth) == 0 then 
      search_groups counts (depth + 1) m t max_score_ref
    else (
      (* 分支 1: 提取刻子 (AAA) *)
      if counts.(depth) >= 3 then (
        counts.(depth) <- counts.(depth) - 3;
        search_groups counts depth (m + 1) t max_score_ref;
        counts.(depth) <- counts.(depth) + 3; (* Backtrack *)
      );

      (* 分支 2: 提取顺子 (ABC) - 仅限数牌 *)
      if depth < 27 && (depth mod 9) < 7 then (
        if counts.(depth) > 0 && counts.(depth+1) > 0 && counts.(depth+2) > 0 then (
          counts.(depth) <- counts.(depth) - 1;
          counts.(depth+1) <- counts.(depth+1) - 1;
          counts.(depth+2) <- counts.(depth+2) - 1;
          search_groups counts depth (m + 1) t max_score_ref;
          counts.(depth) <- counts.(depth) + 1; (* Backtrack *)
          counts.(depth+1) <- counts.(depth+1) + 1;
          counts.(depth+2) <- counts.(depth+2) + 1;
        )
      );

      (* 分支 3: 提取搭子 (对子 AA) *)
      if counts.(depth) >= 2 then (
        counts.(depth) <- counts.(depth) - 2;
        search_groups counts depth m (t + 1) max_score_ref;
        counts.(depth) <- counts.(depth) + 2; (* Backtrack *)
      );

      (* 分支 4: 提取搭子 (顺子搭子 AB / AC) *)
      if depth < 27 && (depth mod 9) < 8 then (
        (* 两面/边张 (AB) *)
        if counts.(depth+1) > 0 then (
          counts.(depth) <- counts.(depth) - 1;
          counts.(depth+1) <- counts.(depth+1) - 1;
          search_groups counts depth m (t + 1) max_score_ref;
          counts.(depth) <- counts.(depth) + 1; (* Backtrack *)
          counts.(depth+1) <- counts.(depth+1) + 1;
        );
        (* 嵌张 (AC) *)
        if (depth mod 9) < 7 && counts.(depth+2) > 0 then (
          counts.(depth) <- counts.(depth) - 1;
          counts.(depth+2) <- counts.(depth+2) - 1;
          search_groups counts depth m (t + 1) max_score_ref;
          counts.(depth) <- counts.(depth) + 1; (* Backtrack *)
          counts.(depth+2) <- counts.(depth+2) + 1;
        )
      );

      (* 分支 5: 都不选，作为孤张跳过 *)
      (* 这里的关键是：必须要把 counts.(depth) 消耗掉或者跳过，防止死循环。
         在这个 DFS 结构里，我们是基于 depth 递增的。
         如果我们不把当前的牌用掉（上述分支都走不通，或者选择不走），
         我们不仅要 depth + 1，还意味着我们放弃了利用当前的牌。
         
         但是，上面的逻辑里我们没有减少 counts.(depth) 就进入了下一层？
         不对。上面的分支都是“用掉一部分牌，然后递归 depth (继续看当前张够不够再组)”
         或者 “递归 depth + 1”。
         
         正确的逻辑：
         如果不做任何操作，就意味着当前所有的 counts.(depth) 都变成了孤张（废牌）。
         所以直接进 depth + 1。
      *)
      search_groups counts (depth + 1) m t max_score_ref
    )
  )

(* 3. 向听数计算主入口 *)
let calculate_shanten (hand: t) : int =
  let counts = to_frequency_table hand in
  let min_shanten = ref 8 in (* 初始最大值 *)

  (* 辅助：计算标准型的分值 *)
  (* 公式：8 - 2*M - T - P *)
  let get_standard_shanten with_pair =
    let max_score = ref (-1) in
    (* 这里传入 0,0 开始搜面子和搭子 *)
    search_groups counts 0 0 0 max_score;
    (* !max_score = 2*m + t *)
    let pair_val = if with_pair then 1 else 0 in
    8 - !max_score - pair_val
  in

  (* A. 七对子 (Seven Pairs) 判定 *)
  let pairs = ref 0 in
  for i = 0 to 33 do if counts.(i) >= 2 then pairs := !pairs + 1 done;
  let shanten_7 = 6 - !pairs in
  if shanten_7 < !min_shanten then min_shanten := shanten_7;

  (* B. 标准型 - 枚举雀头 *)
  for i = 0 to 33 do
    if counts.(i) >= 2 then (
      counts.(i) <- counts.(i) - 2; (* 拿出一对做雀头 *)
      let s = get_standard_shanten true in
      if s < !min_shanten then min_shanten := s;
      counts.(i) <- counts.(i) + 2; (* 回溯 *)
    )
  done;

  (* C. 标准型 - 无雀头 (单钓) *)
  (* 这种情况下，我们找 4个面子+1个单张，公式里 P=0 *)
  let s_no_pair = get_standard_shanten false in
  if s_no_pair < !min_shanten then min_shanten := s_no_pair;

  !min_shanten

(* 判断胡牌：向听数 -1 *)
let is_complete hand = calculate_shanten hand = -1

(* 4. 进张数 (Ukeire) *)
let calc_ukeire_count (hand_13: t) : int =
  (* 这里的 hand_13 是已经打出一张牌后的手牌 *)
  let current_shanten = calculate_shanten hand_13 in
  let effective_count = ref 0 in
  
  List.iter (fun tile ->
    let temp_hand = add hand_13 tile in
    (* 如果加上这张牌，向听数变小了，说明是有效进张 *)
    if calculate_shanten temp_hand < current_shanten then (
      (* 计算剩余张数 *)
      let count_in_hand = 
        List.filter (fun t -> Tile.compare t tile = 0) hand_13 |> List.length 
      in
      let possible = 4 - count_in_hand in
      if possible > 0 then effective_count := !effective_count + possible
    )
  ) all_tile_types;
  
  !effective_count

(* 5. 牌效计算：推荐打牌 API *)
let calculate_efficiency (hand_14: t) : (Tile.t * int) list =
  (* 注意：这里必须去重，否则同样的牌会计算多次 *)
  let unique_tiles = List.sort_uniq Tile.compare hand_14 in
  
  let results = 
    List.map (fun tile ->
      match remove_first hand_14 tile with
      | None -> (tile, -1)
      | Some hand_13 -> 
          let ukeire = calc_ukeire_count hand_13 in
          (tile, ukeire)
    ) unique_tiles
  in
  
  (* 按进张数降序排列 *)
  List.sort (fun (_, c1) (_, c2) -> compare c2 c1) results

(* 兼容接口 *)
let possible_sets _ = []