(* lib/astar.ml *)

(* lib/astar.ml *)

open Core

(** [Searchable] Module Type *)
module type Searchable = sig
  type t
  val compare : t -> t -> int
  val heuristic : t -> float
  val neighbors : t -> (t * float) list
  val is_goal : t -> bool
end

(** [Make] Functor *)
module Make (S : Searchable) = struct

  (** Internal representation of a search node. *)
  type node = {
    state : S.t;
    g : float; (** g(n): The actual cost from the start node to this node. *)
    f : float; (** f(n) = g(n) + h(n): The estimated total cost of the path through this node. *)
  }

  (** [sort_queue q]
      Sorts the priority queue based on the 'f' score in ascending order.
      Nodes with lower estimated total costs are prioritized.
      Refactored to use Core's [List.sort] with labeled [~compare] argument. *)
  let sort_queue q =
    List.sort q ~compare:(fun a b ->
      (* First compare by f-score (estimated total cost) *)
      let cmp_f = Float.compare a.f b.f in
      if cmp_f <> 0 then cmp_f
      else
        (* Tie-breaking: prefer nodes with higher g-score (closer to goal geometrically)
           or just verify g for stability. *)
        Float.compare a.g b.g
    )

  (** [search start_state] *)
  let search start_state =
    (* Main recursive loop processing the open_set (priority queue) *)
    let rec loop open_set closed_set =
      match open_set with
      | [] -> None (* The queue is empty, search failed *)
      | current :: rest ->

          (* 1. Goal Check *)
          if S.is_goal current.state then
            Some (current.g, current.state)
          else
            (* 2. Closed Set Check: Have we visited this state with a lower or equal cost? *)
            (* Using Core's [List.exists] with labeled argument [~f] *)
            if List.exists closed_set ~f:(fun s -> S.compare s current.state = 0) then
              loop rest closed_set
            else
              (* 3. Expand Neighbors *)
              let next_nodes =
                S.neighbors current.state
                (* Core's [List.map] uses labeled argument [~f] *)
                |> List.map ~f:(fun (next_state, step_cost) ->
                    let new_g = current.g +. step_cost in
                    {
                      state = next_state;
                      g = new_g;
                      f = new_g +. S.heuristic next_state (* f = g + h *)
                    }
                )
              in

              (* 4. Update Open Set: Add new nodes and re-sort *)
              (* Note: Ideally, a dedicated Priority Queue data structure should be used
                 for O(log n) insertions, but List sort is acceptable for this scope. *)
              let new_open = sort_queue (rest @ next_nodes) in

              (* Continue search, adding current state to closed set *)
              loop new_open (current.state :: closed_set)
    in

    let initial_node = {
      state = start_state;
      g = 0.0;
      f = S.heuristic start_state
    } in

    loop [initial_node] []
end