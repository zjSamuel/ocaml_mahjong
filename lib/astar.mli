(** lib/astar.mli *)

(** [1] 定义接口：描述一个可以被搜索的问题 *)
module type Searchable = sig
  type t (* 状态类型 *)

  val compare : t -> t -> int
  (** 比较函数，用于排序或判重 *)

  val heuristic : t -> float
  (** 启发式函数 h(n)：预估到达目标的剩余代价 *)

  val neighbors : t -> (t * float) list
  (** 获取邻居节点：返回 (新状态, 移动代价) 的列表 *)

  val is_goal : t -> bool
  (** 判断是否到达目标 *)
end

(** [2] Functor：输入一个 Searchable 模块，输出一个求解器 *)
module Make (S : Searchable) : sig
  val search : S.t -> (float * S.t) option
  (** A* 搜索主函数
      @param start_state 初始状态
      @return Some (总代价, 最终状态) 如果找到解; None 如果无解 *)
end
