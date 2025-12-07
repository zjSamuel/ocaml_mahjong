type t = {
  name : string;
  hand : Hand.t;
  discards : Tile.t list;
  last_drawn : Tile.t option;
  melds : Hand.meld list; (* 使用 Hand.meld *)
}

let create name =
  { name; hand = Hand.empty; discards = []; last_drawn = None; melds = [] }

let name p = p.name
let hand p = p.hand
let discards p = p.discards
let last_drawn p = p.last_drawn
let melds p = p.melds

let sort_tiles = List.sort Tile.compare

let is_sequence (t1: Tile.t) (t2: Tile.t) (t3: Tile.t) : bool =
  match (t1, t2, t3) with
  | (Tile.Numbered(s1, n1), Tile.Numbered(s2, n2), Tile.Numbered(s3, n3)) ->
      begin
        s1 = s2 && s2 = s3 &&
        let sorted = List.sort compare [n1; n2; n3] in
        match sorted with
        | [a; b; c] -> a + 1 = b && b + 1 = c
        | _ -> false
      end
  | _ -> false

let find_chi_options (p: t) (target: Tile.t) : (Tile.t * Tile.t) list =
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

let can_pon (p: t) (target: Tile.t) : bool =
  let count = List.filter (fun t -> Tile.compare t target = 0) p.hand |> List.length in
  count >= 2

let can_kan (p: t) (target: Tile.t) : bool =
  let count = List.filter (fun t -> Tile.compare t target = 0) p.hand |> List.length in
  count >= 3

let can_ron (p: t) (target: Tile.t) : bool =
  Hand.is_complete (Hand.add p.hand target)

let can_tsumo (p: t) : bool =
  Hand.is_complete p.hand

(* 示例修改 perform_chi *)
let perform_chi (p: t) (target: Tile.t) (t1: Tile.t) (t2: Tile.t) : t option =
  if not (is_sequence t1 t2 target) then None else
    match Hand.remove_first p.hand t1 with
    | None -> None
    | Some h1 -> match Hand.remove_first h1 t2 with
      | None -> None
      | Some h2 ->
          let sorted = sort_tiles [t1; t2; target] in
          (* [关键] 这里必须是 Hand.Chi *)
          let m = match sorted with [a;b;c] -> Hand.Chi(a,b,c) | _ -> Hand.Chi(t1,t2,target) in 
          Some { p with hand = h2; melds = m :: p.melds; last_drawn = None }

let perform_pon (p: t) (target: Tile.t) : t option =
  match Hand.remove_first p.hand target with
  | None -> None
  | Some h1 ->
      match Hand.remove_first h1 target with
      | None -> None
      | Some h2 ->
          let m = Hand.Pon(target, target, target) in
          Some { p with hand = h2; melds = m :: p.melds; last_drawn = None }

let perform_kan (p: t) (target: Tile.t) : t option =
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
       let m = Hand.Kan(target, target, target, target) in
       Some { p with hand = new_hand; melds = m :: p.melds; last_drawn = None }

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

let add_drawn_tile p tile = { p with hand = Hand.add p.hand tile; last_drawn = Some tile }

let to_string p =
  let melds_str = p.melds |> List.map (function
    | Hand.Chi(a,b,c) -> Printf.sprintf "[吃 %s%s%s]" (Tile.to_string a) (Tile.to_string b) (Tile.to_string c)
    | Pon(a,_,_) -> Printf.sprintf "[碰 %s]" (Tile.to_string a)
    | Kan(a,_,_,_) -> Printf.sprintf "[杠 %s]" (Tile.to_string a)
  ) |> String.concat " " in
  Printf.sprintf "[%s] 手牌:%s 副露:%s" p.name (Hand.to_string p.hand) melds_str

let debug_set_hand p tiles = { p with hand = tiles; melds = []; last_drawn = None }

let tile_count p =
  let hand_n = List.length p.hand in
  let meld_n = List.length p.melds in
  hand_n + (meld_n * 3)

let has_full_hand p = tile_count p >= 14

let get_recommendations p visible_counts =
  if has_full_hand p then
    Hand.get_recommendations_astar p.hand visible_counts
  else
    []