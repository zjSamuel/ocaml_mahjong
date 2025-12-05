(* lib/player.ml *)

(* 1. 定义更丰富的副露类型 *)
type meld = 
  | Chi of Tile.t * Tile.t * Tile.t (* 顺子 *)
  | Pon of Tile.t * Tile.t * Tile.t (* 刻子 *)
  | Kan of Tile.t * Tile.t * Tile.t * Tile.t (* 杠 *)

type t = {
  name : string;
  hand : Hand.t;
  discards : Tile.t list;
  last_drawn : Tile.t option;
  melds : meld list;
  (* is_riichi : bool; (* 未来扩展用 *) *)
}

let create name =
  { name; hand = Hand.empty; discards = []; last_drawn = None; melds = [] }

(* 基本访问器 *)
let name p = p.name
let hand p = p.hand
let discards p = p.discards
let last_drawn p = p.last_drawn
let melds p = p.melds

(* 基础辅助：排序 *)
let sort_tiles = List.sort Tile.compare

(* --- 核心判定逻辑 --- *)

(* A. 判定是否顺子 *)
let is_sequence (t1: Tile.t) (t2: Tile.t) (t3: Tile.t) : bool =
  match (t1, t2, t3) with
  | (Tile.Numbered(s1, n1), Tile.Numbered(s2, n2), Tile.Numbered(s3, n3)) ->
      (* === 关键修改：加上 begin ... end 包裹内部逻辑 === *)
      begin
        s1 = s2 && s2 = s3 &&
        let sorted = List.sort compare [n1; n2; n3] in
        match sorted with
        | [a; b; c] -> a + 1 = b && b + 1 = c
        | _ -> false
      end
      (* ============================================== *)
  | _ -> false

(* B. 查找所有可能的“吃”组合 *)
let find_chi_options (p: t) (target: Tile.t) : (Tile.t * Tile.t) list =
  (* 只有数牌才能吃 *)
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
                else
                  pair_with_h1 inner_acc t2
          in
          find (pair_with_h1 acc t) t
    in
    find [] uniq_hand

(* C. 查找是否可以“碰” *)
(* 碰的逻辑很简单：手里至少有 2 张和 target 一样的牌 *)
let can_pon (p: t) (target: Tile.t) : bool =
  let count = List.filter (fun t -> Tile.compare t target = 0) p.hand |> List.length in
  count >= 2

(* D. 查找是否可以“杠” (明杠) *)
(* 杠的逻辑：手里至少有 3 张和 target 一样的牌 *)
let can_kan (p: t) (target: Tile.t) : bool =
  let count = List.filter (fun t -> Tile.compare t target = 0) p.hand |> List.length in
  count >= 3

(* E. 判定是否“荣和” (点炮) *)
let can_ron (p: t) (target: Tile.t) : bool =
  Hand.is_complete (Hand.add p.hand target)

(* F. 判定是否“自摸” *)
let can_tsumo (p: t) : bool =
  Hand.is_complete p.hand

(* --- 核心执行逻辑 (State Mutation) --- *)

(* 1. 执行吃 *)
let perform_chi (p: t) (target: Tile.t) (t1: Tile.t) (t2: Tile.t) : t option =
  match Hand.remove_first p.hand t1 with
  | None -> None
  | Some hand_minus_t1 ->
      match Hand.remove_first hand_minus_t1 t2 with
      | None -> None
      | Some final_hand ->
          let sorted = sort_tiles [t1; t2; target] in
          let m = match sorted with [a;b;c] -> Chi(a,b,c) | _ -> Chi(t1,t2,target) in
          Some { p with hand = final_hand; melds = m :: p.melds; last_drawn = None }

(* 2. 执行碰 *)
let perform_pon (p: t) (target: Tile.t) : t option =
  (* 移除两张 target *)
  match Hand.remove_first p.hand target with
  | None -> None
  | Some h1 ->
      match Hand.remove_first h1 target with
      | None -> None
      | Some h2 ->
          let m = Pon(target, target, target) in
          Some { p with hand = h2; melds = m :: p.melds; last_drawn = None }

(* 3. 执行杠 (明杠) *)
let perform_kan (p: t) (target: Tile.t) : t option =
  (* 移除三张 target *)
  let rec remove_3 h count acc =
    if count = 0 then Some (List.rev acc @ h)
    else match h with
      | [] -> None
      | x :: xs -> 
          if Tile.compare x target = 0 then remove_3 xs (count - 1) acc
          else remove_3 xs count (x :: acc)
  in
  match remove_3 p.hand 3 [] with
  | None -> None
  | Some new_hand ->
       let m = Kan(target, target, target, target) in
       Some { p with hand = new_hand; melds = m :: p.melds; last_drawn = None }

(* 常规操作 *)
let draw_tile p deck =
  match Deck.draw deck with
  | None -> None
  | Some (tile, next_deck) ->
      Some ({ p with hand = Hand.add p.hand tile; last_drawn = Some tile }, next_deck)

let discard_tile p tile =
  match Hand.remove_first p.hand tile with
  | None -> None
  | Some new_hand -> 
      Some { p with hand = new_hand; discards = tile :: p.discards; last_drawn = None }

(* 辅助：打印 *)
let to_string p =
  let melds_str = p.melds |> List.map (function
    | Chi(a,b,c) -> Printf.sprintf "[吃 %s%s%s]" (Tile.to_string a) (Tile.to_string b) (Tile.to_string c)
    | Pon(a,_,_) -> Printf.sprintf "[碰 %s]" (Tile.to_string a)
    | Kan(a,_,_,_) -> Printf.sprintf "[杠 %s]" (Tile.to_string a)
  ) |> String.concat " " in
  Printf.sprintf "[%s] 手牌:%s 副露:%s" p.name (Hand.to_string p.hand) melds_str

let debug_set_hand p tiles = { p with hand = tiles; melds = []; last_drawn = None }

let tile_count p =
  let hand_n = List.length p.hand in
  let meld_n = List.length p.melds in
  hand_n + (meld_n * 3)

(* 新增：辅助判断状态 *)
let has_full_hand p = tile_count p >= 14

(* 在末尾添加 *)
(* let get_recommendations p =
  (* 只有轮到自己打牌(14张)时才计算，否则没意义 *)
  if has_full_hand p then
    Hand.calculate_efficiency p.hand
  else
    [] *)