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

module Make (S : Searchable) = struct
  type node = {
    state : S.t;
    g : float;
    f : float;
  }

  let sort_queue q =
    List.sort q ~compare:(fun a b ->
        let cmp_f = Float.compare a.f b.f in
        if cmp_f <> 0 then cmp_f
        else
          Float.compare a.g b.g)
  let search start_state =
    let rec loop open_set closed_set =
      match open_set with
      | [] -> None
      | current :: rest ->
          if S.is_goal current.state then Some (current.g, current.state)
          else if
            List.exists closed_set ~f:(fun s -> S.compare s current.state = 0)
          then loop rest closed_set
          else
            let next_nodes =
              S.neighbors current.state
              |> List.map ~f:(fun (next_state, step_cost) ->
                     let new_g = current.g +. step_cost in
                     {
                       state = next_state;
                       g = new_g;
                       f = new_g +. S.heuristic next_state (* f = g + h *);
                     })
            in

            let new_open = sort_queue (rest @ next_nodes) in

            loop new_open (current.state :: closed_set)
    in

    let initial_node =
      { state = start_state; g = 0.0; f = S.heuristic start_state }
    in

    loop [ initial_node ] []
end
