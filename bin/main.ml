(* bin/main.ml [功能更新：显示所有玩家] *)

open Mahjong

(* 1. 全局游戏状态 *)
let game_state_ref = ref (Game.create ())

(****************************************************************************)
(* 2. HTML 渲染函数 (已重写) *)
(****************************************************************************)
let render_html_for_game (game: Game.t) : string =
  
  (* 辅助函数 A: 将弃牌堆转为 HTML 字符串 (无改动) *)
  let discards_to_html (discards: Tile.t list) : string =
    discards
    |> List.map Tile.to_string
    |> String.concat ", "
  in

  (* 辅助函数 B: 渲染 *所有* 玩家的牌和操作 *)
  let render_all_players_html (game: Game.t) : string =
    let current_idx = game.current_player_idx in

    (* 嵌套辅助函数: 渲染 *一个* 玩家的手牌 *)
    let hand_to_html (hand: Tile.t list) (is_current_player: bool) : string =
      hand
      |> List.mapi (fun i tile ->
          if is_current_player then
            (* 仅为当前玩家显示编号，以便他们选择 *)
            Printf.sprintf "<li>(%d) %s</li>" (i + 1) (Tile.to_string tile)
          else
            (* 其他玩家只显示牌面 *)
            Printf.sprintf "<li>%s</li>" (Tile.to_string tile)
        )
      |> String.concat "\n"
    in

    (* 遍历所有玩家 (0, 1, 2, 3) 并生成他们的 HTML 块 *)
    game.players
    |> Array.mapi (fun i player ->
        let is_current = (i = current_idx) in
        let hand_size = List.length (player.Mahjong.Player.hand) in
        
        (* 1. 为当前玩家设置高亮样式 *)
        let style = 
          if is_current then 
            "font-weight: bold; border: 2px solid #007bff; padding: 10px; border-radius: 8px; background-color: #f8f9fa;" 
          else 
            "border: 1px solid #ccc; padding: 10px; border-radius: 8px; background-color: #fafafa; color: #555;" 
        in
        
        (* 2. 获取该玩家的手牌 HTML *)
        let hand_html = hand_to_html player.hand is_current in
        
        (* 3. [关键] 仅为当前玩家生成操作表单 *)
        let action_html =
          if not is_current then "" (* 不是你的回合，没有操作 *)
          else if hand_size = 13 then
            (* 这是你的回合，13 张牌 -> 显示摸牌按钮 *)
            "<p>你有 13 张牌。请摸牌。</p>
             <form action='/draw' method='POST'>
               <button type='submit' style='font-size: 1.2em;'>摸一张牌</button>
             </form>"
          else if hand_size = 14 then
            (* 这是你的回合，14 张牌 -> 显示打牌表单 *)
            "<p>你刚摸了一张牌，现有 14 张。请选择一张打出：</p>
             <form action='/play' method='POST'>
               <label for='discard_index'>打出第 (1-14) 张牌:</label>
               <input type='number' name='discard_index' min='1' max='14' required>
               <button type='submit' style='font-size: 1.2em;'>打牌</button>
             </form>"
          else
            (* 游戏结束 (牌堆空了) *)
            "<h3>游戏结束 (牌堆已空或出错)</h3>"
        in

        (* 4. 将所有部分组合成这个玩家的 HTML 块 *)
        Printf.sprintf
          "<div style='margin-bottom: 20px; %s'>
             <h3>玩家 %d %s</h3>
             <ul>%s</ul>
             %s
           </div>"
          style
          i
          (if is_current then "(轮到你了)" else "")
          hand_html
          action_html
      )
    |> Array.to_list
    |> String.concat "\n" (* 将所有玩家的 HTML 块组合在一起 *)
  in

  (*
    主 HTML 模板 (已更新)
  *)
  Printf.sprintf
    "<html>
      <head>
        <title>OCaml Mahjong</title>
        <style>
          body { font-family: sans-serif; padding: 20px; }
          ul { list-style: none; padding-left: 0; }
          /* 将牌水平排列 */
          li { margin: 5px; font-size: 1.2em; display: inline-block; margin-right: 8px; padding: 5px; border: 1px solid #ddd; border-radius: 4px;}
          .discards { margin-top: 20px; color: #555; }
          .info { font-weight: bold; font-size: 1.2em; }
        </style>
      </head>
      <body>
        <h1>OCaml Mahjong Demo</h1>
        <p class='info'>
          当前玩家: %d | 牌堆剩余: %d
        </p>
        
        <hr>

        %s

        <hr>
        
        <div class='discards'>
          <strong>弃牌堆:</strong> [ %s ]
        </div>

        <p style='margin-top: 30px;'>
          <a href='/new_game'>开始新游戏</a>
        </p>
      </body>
    </html>"
    (* 填充模板 *)
    game.current_player_idx
    (List.length game.deck)
    (render_all_players_html game) (* <-- 改动在这里 *)
    (discards_to_html game.discard_pile)

(****************************************************************************)
(* 3. Dream 服务器主程序 (你的工作版本) *)
(****************************************************************************)
let () = 
  (* 确保 OCaml 记录堆栈跟踪，以防万一 *)
  Printexc.record_backtrace true;

  Dream.run
  @@ Dream.logger

  (* 你的路由 *)
  @@ Dream.router [

    (* GET / : 主页 *)
    Dream.get "/" (fun _ ->
      let current_game = !game_state_ref in
      Dream.html (render_html_for_game current_game)
    );

    (* POST /draw : 摸牌动作 *)
    Dream.post "/draw" (fun request ->
      Printf.eprintf "[DEBUG] draw!!!\n";
      let game = !game_state_ref in
      if (List.length game.players.(game.current_player_idx).hand) <> 13 then
        Dream.redirect request "/"
      else
        let (game_after_draw, drawn_tile_opt) = Game.draw_card game in
        (match drawn_tile_opt with
        | None ->
          game_state_ref := game_after_draw;
          Dream.redirect request "/"
        | Some _ ->
          game_state_ref := game_after_draw;
          Dream.redirect request "/")
    );

    (* POST /play : 打牌动作 *)
    Dream.post "/play" (fun request ->
      Printf.eprintf "[DEBUG] play!!!\n";
      let game_with_14 = !game_state_ref in
      let player = game_with_14.players.(game_with_14.current_player_idx) in

      if (List.length player.hand) <> 14 then (
        Printf.eprintf "[DEBUG] /play 错误: 手牌不是 14 张 (可能是重复提交)。\n";
        Dream.redirect request "/"
      )
      else
        (* 你找到的正确修复方案：在 Dream.form 上禁用 CSRF
        *)
        match%lwt Dream.form ~csrf:false request with
        | `Ok [("discard_index", index_str)] ->
          (
            Printf.eprintf "[DEBUG] /play: 成功收到表单，index_str = \"%s\"\n" index_str;
            match int_of_string_opt index_str with
            | Some n when n >= 1 && n <= 14 ->
              let tile_to_discard = List.nth player.hand (n - 1) in
              Printf.eprintf "[DEBUG] /play: 用户选择索引 %d, 对应牌 %s\n"
                n (Tile.to_string tile_to_discard);
              let (game_after_discard, _) = Game.discard_card game_with_14 tile_to_discard in
              game_state_ref := game_after_discard;
              Printf.eprintf "[DEBUG] /play: 打牌成功。新玩家是 %d\n"
                game_after_discard.current_player_idx;
              Dream.redirect request "/"
            | _ ->
              Printf.eprintf "[DEBUG] /play 错误: 输入无效 '%s'\n" index_str;
              Dream.redirect request "/"
          )

        | `Ok form_data ->
          Printf.eprintf "[DEBUG] /play 错误: 表单字段异常，共 %d 个：\n" (List.length form_data);
          List.iter (fun (k, v) -> Printf.eprintf "  -> (%s, %s)\n" k v) form_data;
          Dream.redirect request "/"

        | _ ->
          Printf.eprintf "[DEBUG] /play 错误: Dream.form 未返回 Ok。\n";
          Dream.redirect request "/"
    );

    (* GET /new_game : 新游戏 *)
    Dream.get "/new_game" (fun request ->
      game_state_ref := Game.create ();
      Dream.redirect request "/"
    );
  ]