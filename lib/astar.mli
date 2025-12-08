(** lib/astar.mli *)

(** [Searchable] Module Type
    Defines the interface required for a state space to be searched using the A* algorithm.
    Any module that implements this signature can be passed to the [Make] functor. *)
module type Searchable = sig
  (** The abstract type representing a state in the search problem (e.g., a board configuration). *)
  type t

  (** [compare t1 t2] returns an integer indicating the ordering of two states.
      - Returns 0 if [t1] is equal to [t2].
      - Returns a negative integer if [t1] is less than [t2].
      - Returns a positive integer if [t1] is greater than [t2].
      This is required for maintaining the 'closed set' to avoid cycles. *)
  val compare : t -> t -> int

  (** [heuristic state] estimates the "cost to go" from [state] to the goal.
      For the A* algorithm to guarantee finding the optimal path (shortest distance),
      this heuristic function must be **admissible**, meaning it never overestimates
      the actual cost to reach the goal.
      - h(n) <= actual_cost(n, goal) *)
  val heuristic : t -> float

  (** [neighbors state] returns a list of adjacent states reachable from the current [state].
      Each element in the list is a tuple: (next_state, step_cost). *)
  val neighbors : t -> (t * float) list

  (** [is_goal state] returns true if the [state] satisfies the goal condition. *)
  val is_goal : t -> bool
end

(** [Make] Functor
    Constructs a generic A* solver module for a given [Searchable] implementation. *)
module Make (S : Searchable) : sig
  (** [search start_state]
      Executes the A* search algorithm starting from [start_state].
      @param start_state The initial configuration of the problem.
      @return [Some (total_cost, final_state)] if a path to the goal is found.
      @return [None] if the priority queue is exhausted without finding a goal. *)
  val search : S.t -> (float * S.t) option
end