(* lib/hand.ml *)

type t = Tile.t list



let empty = []



let add hand tile =

  List.sort Tile.compare (tile :: hand)



let remove_first hand tile =

  let rec aux acc = function

    | [] -> None

    | h :: t ->

        if Tile.compare h tile = 0 then Some (List.rev acc @ t)

        else aux (h :: acc) t

  in

  aux [] hand



let sort = List.sort Tile.compare



let to_string hand =

  hand |> List.map Tile.to_string |> String.concat " "



(* --- 胡牌判定算法 (Backtracking) --- *)



(* 辅助：移除刻子 (AAA) *)

let remove_triplet tiles tile =

  match tiles with

  | t1 :: t2 :: rest when Tile.compare t1 tile = 0 && Tile.compare t2 tile = 0 ->

      Some rest

  | _ -> None



(* 辅助：移除顺子 (ABC) - 仅限数牌 *)

let remove_sequence tiles tile =

  match tile with

  | Tile.Honor _ -> None (* 字牌无顺子 *)

  | Tile.Numbered (s, n) ->

      if n > 7 then None (* 8或9开头的无法组成顺子 *)

      else

        let n2 = Tile.Numbered (s, n + 1) in

        let n3 = Tile.Numbered (s, n + 2) in

        (* 尝试从列表中移除 n2 和 n3 *)

        let rec remove_one target lst acc =

          match lst with

          | [] -> None

          | h :: t ->

              if Tile.compare h target = 0 then Some (List.rev acc @ t)

              else remove_one target t (h :: acc)

        in

        match remove_one n2 tiles [] with

        | None -> None

        | Some rest1 ->

            match remove_one n3 rest1 [] with

            | None -> None

            | Some rest2 -> Some rest2



(* 核心递归检查：是否能组成 4 个面子 *)

let rec check_sets tiles =

  if tiles = [] then true

  else

    let current = List.hd tiles in

    let remaining = List.tl tiles in

   

    (* 尝试作为刻子 *)

    let is_koutsu =

      match remove_triplet remaining current with

      | Some rest -> check_sets rest

      | None -> false

    in

    if is_koutsu then true

    else

      (* 尝试作为顺子 *)

      match remove_sequence remaining current with

      | Some rest -> check_sets rest

      | None -> false



(* 判断是否胡牌：14张牌 (或13+1) *)

let is_complete hand =

  let sorted_hand = sort hand in

  (* 标准胡牌必须是 14 张 (发牌13 + 摸1 或 发牌13 + 荣1) *)

  if List.length hand mod 3 <> 2 then false

  else

    (* 尝试每一个唯一的牌作为雀头 (Pair) *)

    let unique_tiles = List.sort_uniq Tile.compare sorted_hand in

    List.exists (fun head ->

     

      (* 通用的移除两张特定牌的方法 *)

      let rec remove_two lst target found acc =

        match lst with

        | [] -> if found = 2 then Some (List.rev acc) else None

        | h :: t ->

            if found < 2 && Tile.compare h target = 0 then remove_two t target (found + 1) acc

            else remove_two t target found (h :: acc)

      in



      match remove_two sorted_hand head 0 [] with

      | None -> false

      | Some remaining_12 -> check_sets (sort remaining_12)

    ) unique_tiles



let possible_sets _hand =

  (* 这是一个简化的实现，为了满足接口要求。

     实际功能通常用于提示玩家手里现有的顺子或刻子。

     使用 _hand 前缀告诉编译器这个参数是有意忽略的。 *)

  []