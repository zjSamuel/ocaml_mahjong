(** game.mli — 游戏流程控制模块
    负责整体游戏状态的维护与操作，包括发牌、摸牌、打牌和轮转逻辑。
*)

type t
(** 游戏状态的抽象类型。
    实现中包含：
    - [deck]：当前的牌山（Deck.t）
    - [players]：四位玩家（Player.t array）
    - [discard_pile]：弃牌堆
    - [current_player_idx]：当前轮到的玩家编号 (0–3)
*)

(** {1 初始化与状态访问} *)

val create : unit -> t
(** 创建一个新的游戏实例。
    - 自动生成并洗牌；
    - 为每位玩家发 13 张牌；
    - 当前玩家默认为 0 号。 *)

val current_player : t -> Player.t
(** 获取当前轮到的玩家。 *)

val to_string : t -> string
(** 将整个游戏状态转换为字符串，包含：
    - 各玩家手牌
    - 弃牌堆
    - 剩余牌数
    - 当前玩家信息 *)

(** {1 核心流程函数} *)

val draw_card : t -> (t * Tile.t option)
(** 当前玩家从牌山摸一张牌。
    返回值：
    - 新的游戏状态；
    - 若成功摸到，返回 [Some tile]；
    - 若牌山为空，返回 [None]。
*)

val discard_card : t -> Tile.t -> (t * Tile.t option)
(** 当前玩家打出指定的牌，更新弃牌堆并轮到下一位玩家。
    返回值：
    - 新的游戏状态；
    - 若成功打出，返回 [Some tile]；
    - 若打出失败（不在手牌中），返回 [None]。
*)

val next_turn : t -> t
(** 将当前玩家切换为下一个玩家（0 → 1 → 2 → 3 → 0）。 *)

val play_turn : t -> t
(** 进行一个完整回合（摸牌 → 打牌），可用于自动模式或测试。 *)

(** {1 游戏状态检查} *)

val is_over : t -> bool
(** 判断游戏是否结束（例如牌山为空或某人和牌）。 *)

val winner : t -> Player.t option
(** 返回获胜玩家（若有）。当前为占位接口，可在未来加入和牌判定逻辑。 *)
