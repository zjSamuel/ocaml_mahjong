(* bin/main.ml [已添加调试功能] *)
open Mahjong

let game_state_ref = ref (Game.create ())

let render_html_for_game (game: Game.t) : string =
  let player = game.players.(game.current_player_idx) in
  let hand = player.hand in
  let hand_size = List.length hand in
  let hand_to_html (hand: Tile.t list) : string =
    hand
    |> List.mapi (fun i tile ->
        Printf.sprintf "<li>(%d) %s</li>" (i + 1) (Tile.to_string tile)
      )
    |> String.concat "\n"
  in
  let discards_to_html (discards: Tile.t list) : string =
    discards
    |> List.map Tile.to_string
    |> String.concat ", "
  in
  Printf.sprintf
    "<html>
      <head>
        <title>OCaml Mahjong</title>
        <style>
          body { font-family: sans-serif; padding: 20px; }
          ul { list-style: none; padding-left: 0; }
          li { margin: 5px; font-size: 1.2em; }
          .discards { margin-top: 20px; color: #555; }
          .info { font-weight: bold; }
        </style>
      </head>
      <body>
        <h1>OCaml Mahjong Demo</h1>
        <p class='info'>
          轮到玩家: %d | 牌堆剩余: %d
        </p>
        <h3>你的手牌:</h3>
        <ul>
          %s
        </ul>
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
    player.id
    (List.length game.deck)
    (hand_to_html hand)
    (
      if hand_size = 13 then
        "<p>你有 13 张牌。请摸牌。</p>
         <form action='/draw' method='POST'>
           <button type='submit' style='font-size: 1.2em;'>摸一张牌</button>
         </form>"
      else if hand_size = 14 then
        "<p>你刚摸了一张牌，现有 14 张。请选择一张打出：</p>
         <form action='/play' method='POST'>
           <label for='discard_index'>打出第 (1-14) 张牌:</label>
           <input type='number' name='discard_index' min='1' max='14' required>
           <button type='submit' style='font-size: 1.2em;'>打牌</button>
         </form>"
      else
        "<h3>游戏结束 (牌堆已空或出错)</h3>"
    )
    (discards_to_html game.discard_pile)

(*
  4. Dream 服务器主程序
*)
(*
  4. Dream 服务器主程序 (简单版本)
*)
let () =
  (* 我们仍然使用 Dream.run，
    但我们传递可选参数 ~csrf:false 来禁用 CSRF 保护 
  *)
  Dream.run ~csrf:false
  @@ Dream.logger
  @@ Dream.router [

    (* GET / : 主页 *)
    Dream.get "/" (fun _ ->
      let current_game = !game_state_ref in
      Dream.html (render_html_for_game current_game)
    );

    (* POST /draw : 摸牌动作 *)
    Dream.post "/draw" (fun request ->
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
          Dream.redirect request "/"
        )
    );

    (* POST /play : 打牌动作 *)
    Dream.post "/play" (fun request ->
      let game_with_14 = !game_state_ref in
      let player = game_with_14.players.(game_with_14.current_player_idx) in

      if (List.length player.hand) <> 14 then (
        Printf.eprintf "[DEBUG] /play 错误: 手牌不是 14 张 (可能是重复提交)。\n";
        Dream.redirect request "/"
      )
      else
        (* 1. 从 Web 表单中解析用户输入 *)
        match%lwt Dream.form request with
        | `Ok [("discard_index", index_str)] ->
          (
            Printf.eprintf "[DEBUG] /play: 成功收到表单，index_str = \"%s\"\n" index_str;
            
            (* 2. 将字符串 "1" 转换为索引 0 *)
            match int_of_string_opt index_str with
            | Some n when n >= 1 && n <= 14 ->
              (* 3. 从手牌中获取那张牌 *)
              let tile_to_discard = List.nth player.hand (n - 1) in
              
              Printf.eprintf "[DEBUG] /play: 用户选择索引 %d, 对应牌 %s\n" n (Tile.to_string tile_to_discard);
              
              (* 4. 执行打牌逻辑 *)
              let (game_after_discard, _) = Game.discard_card game_with_14 tile_to_discard in
              
              (* 5. 更新全局状态 (现在是下一家的13张牌) *)
              game_state_ref := game_after_discard;
              
              Printf.eprintf "[DEBUG] /play: 打牌成功。新玩家是 %d\n" game_after_discard.current_player_idx;
              
              (* 6. 重定向回主页 *)
              Dream.redirect request "/"
            | _ ->
              (* 输入的不是 1-14 的数字 *)
              Printf.eprintf "[DEBUG] /play 错误: 'int_of_string_opt' 失败或数字越界。输入 = \"%s\"\n" index_str;
              Dream.redirect request "/"
          )
        
        | `Ok form_data ->
            Printf.eprintf "[DEBUG] /play 错误: 表单 'Ok' 但不匹配 [('discard_index', ...)] 模式。\n";
            Printf.eprintf "[DEBUG] /play: 实际收到的表单有 %d 个字段：\n" (List.length form_data);
            List.iter (fun (key, value) ->
              Printf.eprintf "    -> 字段: (%s, %s)\n" key value
            ) form_data;
            Dream.redirect request "/"

        | _ ->
          Printf.eprintf "[DEBUG] /play 错误: 'Dream.form' 返回的不是 `Ok`。\n";
          Dream.redirect request "/"
    );

    (* GET /new_game : 重置游戏 *)
    Dream.get "/new_game" (fun request ->
      game_state_ref := Game.create ();
      Dream.redirect request "/"
    );
  ]