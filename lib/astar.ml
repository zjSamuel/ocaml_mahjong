(* lib/astar.ml *)

(** [1] Searchable 接口：描述一个可以被搜索的问题 *)
module type Searchable = sig
  type t (* 节点状态类型 *)

  (* 比较函数，用于排序或去重 *)
  val compare : t -> t -> int

  (* 启发式函数 h(n)：预估到达目标的剩余代价 *)
  (* A* 倾向于寻找最小代价 f = g + h *)
  val heuristic : t -> float

  (* 获取邻居节点：返回 (新状态, 移动代价) 的列表 *)
  val neighbors : t -> (t * float) list

  (* 判断是否到达目标 *)
  val is_goal : t -> bool
end

(** [2] Functor：输入一个 Searchable 模块，输出一个求解器 *)
module Make (S : Searchable) = struct
  (* 内部节点结构 *)
  type node = { state : S.t; g : float; (* 已花费代价 *) f : float (* 预估总代价 *) }

  (* 简单的优先队列排序 (f 值越小越优先) *)
  let sort_queue q =
    List.sort
      (fun a b ->
        let cmp_f = compare a.f b.f in
        if cmp_f <> 0 then cmp_f else compare a.g b.g)
      q

  (* A* 搜索主函数 *)
  (* 返回: Some (最终代价 g, 最终状态 state) | None *)
  let search start_state =
    let rec loop open_set closed_set =
      match open_set with
      | [] -> None (* 搜索失败 *)
      | current :: rest ->
          (* 1. 到达目标？直接返回 *)
          if S.is_goal current.state then Some (current.g, current.state)
          else if
            (* 2. 是否已访问过更优状态？(简单判重) *)
            List.exists (fun s -> S.compare s current.state = 0) closed_set
          then loop rest closed_set
          else
            (* 3. 扩展邻居 *)
            let next_nodes =
              S.neighbors current.state
              |> List.map (fun (next_state, step_cost) ->
                     let new_g = current.g +. step_cost in
                     {
                       state = next_state;
                       g = new_g;
                       f = new_g +. S.heuristic next_state;
                     })
            in
            (* 4. 加入队列并重排 *)
            let new_open = sort_queue (rest @ next_nodes) in
            loop new_open (current.state :: closed_set)
    in

    let initial_node =
      { state = start_state; g = 0.0; f = S.heuristic start_state }
    in
    loop [ initial_node ] []
end
