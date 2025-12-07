type difficulty =
  | Easy (* 随机弃牌 *)
  | Medium (* Pure A* (只看进张) *)
  | Hard (* Enhanced A* (进张 + 打点/防守) *)

type t = {
  name : string;
  hand : Hand.t;
  discards : Tile.t list;
  last_drawn : Tile.t option;
  melds : Hand.meld list; (* 使用 Hand.meld *)
  difficulty : difficulty; (* [新增] 字段 *)
}

(* 更新 create，默认为 Medium *)
let create name =
  {
    name;
    hand = Hand.empty;
    discards = [];
    last_drawn = None;
    melds = [];
    difficulty = Medium;
  }

(* 访问器 *)
let difficulty p = p.difficulty
let set_difficulty p d = { p with difficulty = d }
let name p = p.name
let hand p = p.hand
let discards p = p.discards
let last_drawn p = p.last_drawn
let melds p = p.melds
let sort_tiles = List.sort Tile.compare

let is_sequence (t1 : Tile.t) (t2 : Tile.t) (t3 : Tile.t) : bool =
  match (t1, t2, t3) with
  | Tile.Numbered (s1, n1), Tile.Numbered (s2, n2), Tile.Numbered (s3, n3) -> (
      s1 = s2 && s2 = s3
      &&
      let sorted = List.sort compare [ n1; n2; n3 ] in
      match sorted with [ a; b; c ] -> a + 1 = b && b + 1 = c | _ -> false)
  | _ -> false

let find_chi_options (p : t) (target : Tile.t) : (Tile.t * Tile.t) list =
  match target with
  | Tile.Honor _ -> []
  | _ ->
      let uniq_hand = List.sort_uniq Tile.compare p.hand in
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

let can_pon (p : t) (target : Tile.t) : bool =
  let count =
    List.filter (fun t -> Tile.compare t target = 0) p.hand |> List.length
  in
  count >= 2

let can_kan (p : t) (target : Tile.t) : bool =
  let count =
    List.filter (fun t -> Tile.compare t target = 0) p.hand |> List.length
  in
  count >= 3

let can_ron (p : t) (target : Tile.t) : bool =
  Hand.is_complete (Hand.add p.hand target)

let can_tsumo (p : t) : bool = Hand.is_complete p.hand

(* 示例修改 perform_chi *)
let perform_chi (p : t) (target : Tile.t) (t1 : Tile.t) (t2 : Tile.t) : t option
    =
  if not (is_sequence t1 t2 target) then None
  else
    match Hand.remove_first p.hand t1 with
    | None -> None
    | Some h1 -> (
        match Hand.remove_first h1 t2 with
        | None -> None
        | Some h2 ->
            let sorted = sort_tiles [ t1; t2; target ] in
            (* [关键] 这里必须是 Hand.Chi *)
            let m =
              match sorted with
              | [ a; b; c ] -> Hand.Chi (a, b, c)
              | _ -> Hand.Chi (t1, t2, target)
            in
            Some { p with hand = h2; melds = m :: p.melds; last_drawn = None })

let perform_pon (p : t) (target : Tile.t) : t option =
  match Hand.remove_first p.hand target with
  | None -> None
  | Some h1 -> (
      match Hand.remove_first h1 target with
      | None -> None
      | Some h2 ->
          let m = Hand.Pon (target, target, target) in
          Some { p with hand = h2; melds = m :: p.melds; last_drawn = None })

let perform_kan (p : t) (target : Tile.t) : t option =
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
    p.melds
    |> List.map (function
         | Hand.Chi (a, b, c) ->
             Printf.sprintf "[吃 %s%s%s]" (Tile.to_string a) (Tile.to_string b)
               (Tile.to_string c)
         | Pon (a, _, _) -> Printf.sprintf "[碰 %s]" (Tile.to_string a)
         | Kan (a, _, _, _) -> Printf.sprintf "[杠 %s]" (Tile.to_string a))
    |> String.concat " "
  in
  Printf.sprintf "[%s] 手牌:%s 副露:%s" p.name (Hand.to_string p.hand) melds_str

let debug_set_hand p tiles =
  { p with hand = tiles; melds = []; last_drawn = None }

let tile_count p =
  let hand_n = List.length p.hand in
  let meld_n = List.length p.melds in
  hand_n + (meld_n * 3)

let has_full_hand p = tile_count p >= 14

(* ========================================================== *)
(* AI Heuristics: 静态评估函数 (用于增强版 AI)               *)
(* ========================================================== *)

(* 判断是否为幺九牌 *)
let is_terminal_or_honor = function
  | Tile.Honor _ -> true
  | Tile.Numbered (_, n) -> n = 1 || n = 9

(* 1. 宝牌价值评估: 统计手牌中 Dora 的数量 *)
let eval_dora hand dora_indicators =
  let doras = List.map Tile.next_dora dora_indicators in
  List.fold_left
    (fun acc t ->
      let matches = List.filter (fun d -> Tile.compare t d = 0) doras in
      acc + List.length matches)
    0 hand

(* 2. 断幺九倾向评估: 如果手牌中幺九牌越少，分值越高 *)
let eval_tanyao hand =
  let terminals = List.filter is_terminal_or_honor hand in
  let count = List.length terminals in
  if count = 0 then 2.0 (* 已经纯断幺 *)
  else if count <= 2 then 1.0 (* 很容易做成断幺 *)
  else 0.0 (* 幺九太多，放弃断幺 *)

(* 3. 役牌倾向评估: 役牌对子/刻子加分 *)
let eval_yakuhai hand =
  let counts = Array.make 7 0 in
  List.iter
    (function
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
      | _ -> ())
    hand;
  let score = ref 0.0 in
  (* 白发中 (Index 4, 5, 6) *)
  for i = 4 to 6 do
    if counts.(i) >= 3 then score := !score +. 2.0 (* 刻子: 2分 *)
    else if counts.(i) >= 2 then score := !score +. 1.0 (* 对子: 1分 *)
  done;
  !score

(* 4. 染手倾向评估: 某一种花色特别多时加分 *)
let eval_honitsu hand =
  let m_count = ref 0 in
  let p_count = ref 0 in
  let s_count = ref 0 in
  List.iter
    (function
      | Tile.Numbered (Tile.Man, _) -> incr m_count
      | Tile.Numbered (Tile.Pin, _) -> incr p_count
      | Tile.Numbered (Tile.Sou, _) -> incr s_count
      | Tile.Honor _ -> ())
    hand;
  let max_suit = max !m_count (max !p_count !s_count) in
  (* 如果单一花色超过 8 张，开始给予染手加分 *)
  if max_suit >= 8 then float_of_int (max_suit - 7) *. 0.5 else 0.0

(* 综合评分函数: 输入切牌后的残留手牌，输出该手牌的"潜力分" *)
let evaluate_hand_potential hand dora_inds =
  let score = ref 0.0 in
  (* 权重系数配置 *)
  let w_dora = 1.5 in
  let w_tanyao = 1.0 in
  let w_yakuhai = 1.0 in
  let w_honitsu = 1.0 in

  score := !score +. (float_of_int (eval_dora hand dora_inds) *. w_dora);
  score := !score +. (eval_tanyao hand *. w_tanyao);
  score := !score +. (eval_yakuhai hand *. w_yakuhai);
  score := !score +. (eval_honitsu hand *. w_honitsu);
  !score

(* ========================================================== *)
(* AI API Implementation                                      *)
(* ========================================================== *)

(** [纯净版] 仅基于进张数 (Ukeire) 排序 *)
let get_recommendations_pure p visible_counts =
  if has_full_hand p then
    (* 直接调用 Hand 模块的 A* 算法，不做额外处理 *)
    Hand.get_recommendations_astar p.hand visible_counts
  else []

(** [增强版] 进张数 + 潜力分 混合排序 *)
let get_recommendations_enhanced p visible_counts dora_indicators =
  if has_full_hand p then
    (* 1. 获取所有不退向听的切牌候补 *)
    let candidates = Hand.get_recommendations_astar p.hand visible_counts in

    (* 2. 对每个候补进行评分 *)
    let weighted_candidates =
      List.map
        (fun (tile_to_discard, ukeire) ->
          (* 模拟切掉这张牌后，剩下的 13 张牌 *)
          let hand_after_discard =
            match Hand.remove_first p.hand tile_to_discard with
            | Some h -> h
            | None -> p.hand
          in

          (* 计算剩余手牌的潜力 *)
          let potential_score =
            evaluate_hand_potential hand_after_discard dora_indicators
          in

          (* 最终分 = 进张数 (速度) + 潜力分 (打点) *)
          (* 这里 1.0 是速度的权重，可以调节 *)
          let final_score = float_of_int ukeire +. potential_score in

          (tile_to_discard, ukeire, final_score))
        candidates
    in

    (* 3. 按最终分数降序排序 *)
    List.sort (fun (_, _, s1) (_, _, s2) -> compare s2 s1) weighted_candidates
  else []

let decide_discard (p : t) (visible_counts : int array)
    (dora_indicators : Tile.t list) : Tile.t option =
  if not (has_full_hand p) then None
  else
    match p.difficulty with
    | Easy ->
        (* 初级：完全随机切牌 *)
        let n = List.length p.hand in
        if n = 0 then None else Some (List.nth p.hand (Random.int n))
    | Medium -> (
        (* 中级：Pure A* (只看进张效率) *)
        let recs = get_recommendations_pure p visible_counts in
        match recs with
        | (t, _) :: _ -> Some t
        | [] ->
            (* 如果没有建议（极少情况），随机切 *)
            let n = List.length p.hand in
            if n = 0 then None else Some (List.nth p.hand (Random.int n)))
    | Hard -> (
        (* 高级：Enhanced A* (进张 + 打点潜力) *)
        let recs =
          get_recommendations_enhanced p visible_counts dora_indicators
        in
        match recs with
        | (t, _, _) :: _ -> Some t
        | [] -> (
            (* Fallback to Medium *)
            let recs_pure = get_recommendations_pure p visible_counts in
            match recs_pure with
            | (t, _) :: _ -> Some t
            | [] -> Some (List.nth p.hand (Random.int (List.length p.hand)))))
