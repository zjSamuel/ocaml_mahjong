(** hand.mli — Hand Structure and Helper Operations *)

type t = Tile.t list

(* [新增] 将 meld 定义移动到这里 *)
type meld = 
  | Chi of Tile.t * Tile.t * Tile.t
  | Pon of Tile.t * Tile.t * Tile.t
  | Kan of Tile.t * Tile.t * Tile.t * Tile.t

val empty : t
val add : t -> Tile.t -> t
val remove_first : t -> Tile.t -> t option
val sort : t -> t
val to_string : t -> string
val is_complete : t -> bool
val possible_sets : t -> Tile.t list list
val tile_to_id : Tile.t -> int
val calculate_shanten : t -> int
val calculate_efficiency : t -> int array -> (Tile.t * int) list
val get_recommendations_astar : t -> int array -> (Tile.t * int) list

module Score : sig
  type yaku = 
    | MenzenTsumo | Riichi | Ippatsu
    | Pinfu | Tanyao | Iipeiko | Yakuhai of string | Rinshan
    | Sanshoku | Itsu | Chanta | Chiitoitsu | Toitoi | Sanankou | Sankantsu | SanshokuDoukou | Honroutou | Shousangen
    | Honitsu | Junchan | Ryanpeiko
    | Chinitsu
    | Dora of int
  
  type result = {
    han: int;
    yaku_list: yaku list;
    fu: int;
    points: int;
  }
end

(* [修改] 第二个参数改为 meld list (不再是 Player.meld list) *)
val calculate_score : t -> meld list -> Tile.t list -> Tile.honor -> Tile.honor -> bool -> bool -> Score.result option