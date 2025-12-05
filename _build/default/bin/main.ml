(* bin/main.ml *)
open Mahjong

(* 1. 辅助工具：字符串反解析为 Tile *)
let parse_tile_str s =
  let suits = [Tile.Man; Tile.Pin; Tile.Sou] in
  let nums = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let honors = [Tile.East; Tile.South; Tile.West; Tile.North; Tile.Red; Tile.Green; Tile.White] in
  
  let numbered = List.concat_map (fun s -> List.map (fun n -> Tile.Numbered (s, n)) nums) suits in
  let honored = List.map (fun h -> Tile.Honor h) honors in
  let all_unique_tiles = numbered @ honored in

  (* 在这份列表里查找匹配的牌 *)
  List.find_opt (fun t -> Tile.to_string t = s) all_unique_tiles

let game_state_ref = ref (Game.create ())

(* 2. 渲染逻辑：副露 (Melds) *)
let render_melds (p: Player.t) =
  Player.melds p
  |> List.map (function
      | Player.Chi(a, b, c) ->
          Printf.sprintf 
            "<div class='meld-box'>
               <span class='tile static small'>%s</span>
               <span class='tile static small'>%s</span>
               <span class='tile static small active-meld'>%s</span>
             </div>"
            (Tile.to_string a) (Tile.to_string b) (Tile.to_string c)
      | Player.Pon(a, _, _) ->
          Printf.sprintf 
            "<div class='meld-box'>
               <span class='tile static small'>%s</span>
               <span class='tile static small'>%s</span>
               <span class='tile static small active-meld'>%s</span>
             </div>"
            (Tile.to_string a) (Tile.to_string a) (Tile.to_string a)
      | Player.Kan(a, _, _, _) ->
          Printf.sprintf 
            "<div class='meld-box'>
               <span class='tile static small'>%s</span>
               <span class='tile static small'>%s</span>
               <span class='tile static small'>%s</span>
               <span class='tile static small active-meld'>%s</span>
             </div>"
            (Tile.to_string a) (Tile.to_string a) (Tile.to_string a) (Tile.to_string a)
    )
  |> String.concat " "

(* 3. 渲染逻辑：单张手牌 *)
let render_single_tile (tile: Tile.t) (idx: int) (is_clickable: bool) (is_new: bool) =
  let tile_str = Tile.to_string tile in
  let class_str = if is_clickable then "tile clickable" else "tile static" in
  let style_extra = if is_new then "margin-left: 20px; border: 2px solid #fdcb6e; transform: translateY(-8px);" else "" in
  
  if is_clickable then
    Printf.sprintf
      "<form action='/play' method='POST' style='display:inline-block; margin: 4px; %s'>
         <input type='hidden' name='discard_index' value='%d'>
         <button type='submit' class='%s'>%s</button>
       </form>"
      style_extra idx class_str tile_str
  else
    Printf.sprintf "<div class='%s' style='%s'>%s</div>" class_str style_extra tile_str

let render_html (game: Game.t) : string =
  (* A. 数据准备：视角锁定 Player 0 *)
  let all_players = Game.all_players game in
  let human_p = List.nth all_players 0 in 
  
  let current_idx = Game.current_player_id game in
  let is_my_turn = (current_idx = 0) in
  
  (* 状态判断 *)
  let can_discard = is_my_turn && Player.has_full_hand human_p in
  let can_draw = is_my_turn && not (Player.has_full_hand human_p) in

  let full_hand = Player.hand human_p in
  let drawn_opt = Player.last_drawn human_p in
  let last_discard_opt = Game.last_discard game in

  let suggestion_html =
    if can_discard then
      let recommendations = Player.get_recommendations human_p in
      (* 取前 3 名 *)
      let top3 = List.filteri (fun i _ -> i < 3) recommendations in
      
      let rows = 
        top3 |> List.map (fun (tile, count) ->
          Printf.sprintf 
            "<div style='display:flex; align-items:center; margin-bottom:5px;'>
               <span style='margin-right:10px; color:#a29bfe; font-weight:bold;'>打:</span>
               <span class='tile static small'>%s</span>
               <span style='margin-left:10px; color:#ccc; font-size:0.9em;'>(进张: <strong style='color:#fdcb6e'>%d</strong> 张)</span>
             </div>"
            (Tile.to_string tile) count
        ) |> String.concat ""
      in
      if rows = "" then "" 
      else 
        Printf.sprintf 
          "<div style='position:absolute; top:80px; right:20px; width:200px; background:#2d3436; border:1px solid #636e72; padding:10px; border-radius:8px; box-shadow:0 4px 10px rgba(0,0,0,0.3);'>
             <div style='color:#00b894; font-weight:bold; border-bottom:1px solid #636e72; padding-bottom:5px; margin-bottom:10px;'>💡 牌效助手</div>
             %s
           </div>"
          rows
    else ""
  in
  (* B. 动作判定 (吃碰杠荣) *)
  let action_buttons_html =
    match last_discard_opt with
    | None -> ""
    | Some target ->
        let t_str = Tile.to_string target in
        let btns = ref [] in
        let last_discarder = (current_idx - 1 + 4) mod 4 in
        (* 场景 1: 不是我的回合 -> 检测 全局中断 (碰/杠/荣) *)
        if (not is_my_turn) && (last_discarder <> 0) then (
          if Player.can_ron human_p target then 
            btns := (!btns) @ [Printf.sprintf "<form action='/win_ron' method='POST' style='display:inline'><input type='hidden' name='target' value='%s'><button class='win-btn'>⚡ 荣和 (Ron)!</button></form>" t_str];
          
          if Player.can_pon human_p target then 
            btns := (!btns) @ [Printf.sprintf "<form action='/pon' method='POST' style='display:inline; margin-left:10px;'><input type='hidden' name='target' value='%s'><button class='action-btn' style='background:#0984e3'>碰 (Pon)</button></form>" t_str];
          
          if Player.can_kan human_p target then 
            btns := (!btns) @ [Printf.sprintf "<form action='/kan' method='POST' style='display:inline; margin-left:10px;'><input type='hidden' name='target' value='%s'><button class='action-btn' style='background:#6c5ce7'>杠 (Kan)</button></form>" t_str];
        );

        (* 场景 2: 是我的回合(且没摸牌) -> 检测 上家吃 (Chi) *)
        if is_my_turn && (not (Player.has_full_hand human_p)) then (
           let chi_opts = Player.find_chi_options human_p target in
           let chi_btns = chi_opts |> List.map (fun (t1, t2) -> 
             Printf.sprintf "<form action='/chi' method='POST' style='display:inline; margin-left:10px;'><input type='hidden' name='target' value='%s'><input type='hidden' name='t1' value='%s'><input type='hidden' name='t2' value='%s'><button class='action-btn' style='background:#00b894'>吃 %s%s</button></form>" t_str (Tile.to_string t1) (Tile.to_string t2) (Tile.to_string t1) (Tile.to_string t2)
           ) in
           btns := (!btns) @ chi_btns;
        );

        (* 结果生成 *)
        if !btns = [] then "" 
        else (
          let base_html = Printf.sprintf "<div class='action-panel'><h3>👇 检测到牌 [%s]</h3>%s" t_str (String.concat "" !btns) in
          (* 如果不是我的回合但有按钮，必须加一个“跳过”按钮让机器人继续 *)
          if not is_my_turn then
            base_html ^ "<form action='/bot_move' method='POST' style='display:inline; margin-left:20px;'><button class='action-btn' style='background:#b2bec3'>⏭ 跳过 (Pass)</button></form></div>"
          else
            base_html ^ "</div>"
        )
  in

  (* C. 自动跳转脚本 *)
  (* 规则：轮到机器人 AND 人类没有可执行的操作(action_buttons为空) *)
  let should_auto_play = (not is_my_turn) && (action_buttons_html = "") in
  let auto_play_script =
    if should_auto_play then
      "<script>setTimeout(function() { document.getElementById('bot-auto-form').submit(); }, 1000);</script>
       <form id='bot-auto-form' action='/bot_move' method='POST' style='display:none'></form>
       <div style='text-align:center; padding:10px; background:#ffeaa7; color:#d63031; font-weight:bold;'>⏳ 玩家 " ^ (string_of_int current_idx) ^ " 正在思考...</div>"
    else ""
  in

  (* D. 手牌渲染 *)
  let indexed_hand = List.mapi (fun i t -> (t, i + 1)) full_hand in
  let (main_part, special_part) =
    match (can_discard, drawn_opt) with
    | (true, Some new_tile) ->
        let matches = List.filter (fun (t, _) -> Tile.compare t new_tile = 0) indexed_hand in
        (match List.rev matches with
         | (t, idx) :: _ -> (List.filter (fun (_, i) -> i <> idx) indexed_hand, Some (t, idx))
         | [] -> (indexed_hand, None))
    | _ -> (indexed_hand, None)
  in
  let main_hand_html = main_part |> List.map (fun (t, idx) -> render_single_tile t idx can_discard false) |> String.concat "" in
  let special_hand_html = match special_part with | Some (t, idx) -> render_single_tile t idx can_discard true | None -> "" in

  (* E. 对手弃牌区 *)
  let others_discards_html =
    [1; 2; 3] |> List.map (fun i ->
        if i < List.length all_players then
          let p = List.nth all_players i in
          let d_html = Player.discards p |> List.rev |> List.map (fun t -> Printf.sprintf "<span class='tile static small'>%s</span>" (Tile.to_string t)) |> String.concat "" in
          let content = if d_html = "" then "<span style='color:#636e72; font-size:0.8em'>尚未出牌</span>" else d_html in
          let style = if current_idx = i then "border-left: 5px solid #00b894; background: #55efc410;" else "" in
          Printf.sprintf "<div class='other-row' style='%s'><div class='row-label'>%s</div><div class='row-tiles'>%s</div></div>" style (Player.name p) content
        else ""
      ) |> String.concat ""
  in

  (* F. 底部按钮 *)
  let draw_button = if is_my_turn then (if can_draw then "<form action='/draw' method='POST'><button type='submit' class='action-btn draw-btn'>🖐 摸牌</button></form>" else "<div class='info-msg'>请打牌</div>") else "<div class='info-msg' style='color:#b2bec3'>等待其他玩家行动...</div>" in
  let tsumo_button = if is_my_turn && Player.can_tsumo human_p then "<form action='/win' method='POST'><button class='win-btn'>⚡ 自摸!</button></form>" else "" in

  (* G. Debug 区域 *)
  let debug_html = 
    Game.all_players game
    |> List.mapi (fun i p ->
        let is_acting = (i = current_idx) in
        let style = if is_acting then "color: #00b894; font-weight:bold;" else "color: #b2bec3;" in
        Printf.sprintf "<div style='margin-bottom:5px; border-bottom:1px dashed #444;'><span style='%s'>%s %s</span><span style='margin-left:10px'>✋%s</span> <span style='margin-left:10px'>🧊%s</span></div>"
          style (Player.name p) (if is_acting then "(Thinking)" else "") (Hand.to_string (Player.hand p)) (String.concat " " (List.map (fun _ -> "Meld") (Player.melds p)))
      )
    |> String.concat "\n"
  in

  (* H. 最终组装 *)
  Printf.sprintf 
  "<html>
    <head>
      <title>OCaml Mahjong</title>
      <meta charset='utf-8'>
      <style>
        body { font-family: 'Segoe UI', sans-serif; padding: 20px; background-color: #2d3436; color: white; }
        .tile { width: 50px; height: 70px; border: 1px solid #999; border-radius: 6px; background: #fdfdfd; color: #333; font-weight: bold; font-size: 1.1em; display: flex; align-items: center; justify-content: center; box-shadow: 2px 2px 5px rgba(0,0,0,0.3); user-select: none; }
        .clickable { cursor: pointer; transition: transform 0.1s; border-bottom: 4px solid #b2bec3; } .clickable:hover { transform: translateY(-5px); border-bottom-color: #0984e3; color: #0984e3; }
        .static { display: inline-flex; margin: 4px; background: #e0e0e0; border-bottom: 4px solid #b2bec3; color: #636e72; }
        .small { width: 32px; height: 46px; font-size: 0.8em; margin: 2px; }
        .active-meld { border: 2px solid #fdcb6e; }
        .meld-box { display: inline-flex; margin-right: 15px; padding: 5px; background: #636e72; border-radius: 6px; }
        .hand-container { background: #353b48; padding: 20px; border-radius: 10px; min-height: 90px; display: flex; flex-wrap: wrap; align-items: center; border: 1px solid #4b545f;}
        .other-discards-area { background: #353b48; padding: 15px; border-radius: 10px; margin-bottom: 20px; }
        .other-row { display: flex; align-items: center; border-bottom: 1px dashed #636e72; padding: 5px 0; }
        .row-label { width: 80px; font-weight: bold; color: #dfe6e9; font-size: 0.9em; }
        .action-panel { margin-bottom: 20px; padding: 15px; background: #fab1a0; border-radius: 5px; color: #2d3436; box-shadow: 0 0 15px #e17055; }
        .info-box { margin-bottom: 20px; padding: 10px; background: #dfe6e9; color: #2d3436; border-radius: 5px; display: flex; justify-content: space-between; }
        .discards { margin-top: 10px; font-family: monospace; color: #b2bec3; }
        .action-btn { font-size: 1em; padding: 8px 15px; border: none; border-radius: 5px; cursor: pointer; color: white; }
        .draw-btn { background-color: #00b894; font-size: 1.2em; padding: 10px 30px;}
        .win-btn { background-color: #d63031; color: white; font-size: 1.5em; padding: 15px 40px; border: none; border-radius: 50px; cursor: pointer; box-shadow: 0 4px 15px rgba(214, 48, 49, 0.5); animation: pulse 1.5s infinite; }
        .info-msg { color: #ffeaa7; font-weight: bold; font-size: 1.2em; }
        .debug-area { margin-top: 60px; padding: 15px; background-color: #111; border: 1px solid #444; font-family: monospace; font-size: 0.8em; color: #aaa; }
        @keyframes pulse { 0%% { transform: scale(1); } 50%% { transform: scale(1.05); } 100%% { transform: scale(1); } }
        a { color: #74b9ff; text-decoration: none; }
      </style>
    </head>
    <body>
      <h1>🀄 OCaml Mahjong</h1>
      %s %s<div class='info-box'>
        <div><div><strong>当前回合:</strong> %s</div><div><strong>剩余牌山:</strong> %d</div></div>
        <div style='text-align: right'><div><strong>你的副露:</strong> %s</div></div>
      </div>

      <div class='other-discards-area'>
         <div style='color: #b2bec3; margin-bottom: 10px; font-size: 0.9em;'>📺 对手情况</div>
         %s
      </div>

      %s <div class='hand-container'>
        %s <div style='width: 30px;'></div> %s
      </div>
      <div class='discards'><strong>你的牌河:</strong> %s</div>

      <div style='margin-top: 30px; display: flex; gap: 20px; align-items: center;'>
        %s %s
      </div>
      
      <div class='debug-area'><strong>DEBUG:</strong><br/>%s</div>

      <p style='margin-top: 50px; border-top: 1px solid #636e72; padding-top: 10px;'><a href='/new_game'>⟳ Restart</a></p>
    </body>
  </html>"
  auto_play_script
  suggestion_html (* <--- 插入这里 *)
  (Player.name (List.nth all_players current_idx))
  (Game.remaining_tiles game)
  (render_melds human_p)
  others_discards_html
  action_buttons_html
  main_hand_html
  special_hand_html
  (Player.discards human_p |> List.rev |> List.map Tile.to_string |> String.concat " ")
  draw_button
  tsumo_button
  debug_html

(* 5. 路由处理 (保持不变，已包含 chi/pon/kan/win 路由) *)
let () = 
  Printexc.record_backtrace true;
  print_endline "Server starting on http://localhost:3000 ...";
  
  Dream.run ~interface:"0.0.0.0" ~port:3000
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" (fun _ ->
      let game = !game_state_ref in
      Dream.html (render_html game)
    );

    Dream.post "/draw" (fun request ->
      let game = !game_state_ref in
      let (new_game, _tile_opt) = Game.draw_card game in
      game_state_ref := new_game;
      Dream.redirect request "/"
    );

    Dream.post "/play" (fun req -> 
      match%lwt Dream.form ~csrf:false req with 
      | `Ok [("discard_index", i)] -> 
          let idx=int_of_string i in 
          let g = !game_state_ref in 
          let p=Game.current_player g in 
          let hl=Player.hand p in 
          if idx<1||idx>List.length hl then Dream.redirect req "/" 
          else 
            let t=List.nth hl (idx-1) in 
            let (ng1,_) = Game.discard_card g t in 
            (* 注意：这里删除了 Game.auto_play_bots *)
            game_state_ref := ng1; 
            Dream.redirect req "/" 
      | _ -> Dream.redirect req "/"
    );
    
    (* 新增 /bot_move：执行机器人的单步操作 *)
    Dream.post "/bot_move" (fun req ->
      let g = !game_state_ref in
      (* 确保当前不是玩家0 *)
      let current_idx = Game.current_player_id g in
      if current_idx <> 0 then
        let (ng, _) = Game.play_bot_step g in
        game_state_ref := ng;
        Dream.redirect req "/"
      else
        Dream.redirect req "/" (* 如果已经是玩家0，什么都不做直接刷新 *)
    );

    Dream.post "/chi" (fun request ->
      match%lwt Dream.form ~csrf:false request with
      | `Ok form ->
         let get_field k = List.assoc_opt k form in
         (match (get_field "target", get_field "t1", get_field "t2") with
         | (Some ts, Some t1s, Some t2s) ->
             let target = parse_tile_str ts in
             let t1 = parse_tile_str t1s in
             let t2 = parse_tile_str t2s in
             (match (target, t1, t2) with
              | (Some t, Some a, Some b) ->
                  let game = !game_state_ref in
                  let (new_game, success) = Game.perform_chi game t a b in
                  if success then game_state_ref := new_game;
                  Dream.redirect request "/"
              | _ -> Dream.redirect request "/")
         | _ -> Dream.redirect request "/")
      | _ -> Dream.redirect request "/"
    );

    Dream.post "/pon" (fun request ->
      match%lwt Dream.form ~csrf:false request with
      | `Ok form ->
         (match List.assoc_opt "target" form with
         | Some ts ->
             (match parse_tile_str ts with
              | Some t ->
                  let game = !game_state_ref in
                  let (new_game, success) = Game.perform_pon game t in
                  if success then game_state_ref := new_game;
                  Dream.redirect request "/"
              | None -> Dream.redirect request "/")
         | None -> Dream.redirect request "/")
      | _ -> Dream.redirect request "/"
    );

    Dream.post "/kan" (fun request ->
      match%lwt Dream.form ~csrf:false request with
      | `Ok form ->
         (match List.assoc_opt "target" form with
         | Some ts ->
             (match parse_tile_str ts with
              | Some t ->
                  let game = !game_state_ref in
                  print_endline ("Attempting to perform kan on tile: " ^ ts);
                  let (new_game, success) = Game.perform_kan game t in
                  if success then game_state_ref := new_game;
                  Dream.redirect request "/"
              | None -> Dream.redirect request "/")
         | None -> Dream.redirect request "/")
      | _ -> Dream.redirect request "/"
    );

  Dream.post "/win_ron" (fun request ->
      match%lwt Dream.form ~csrf:false request with
      | `Ok form ->
         (match List.assoc_opt "target" form with
         | Some ts ->
             (match parse_tile_str ts with
              | Some tile -> 
                  let game = !game_state_ref in
                  let all_p = Game.all_players game in
                  
                  (* 1. 获胜者：永远是玩家 0 (人类) *)
                  let winner = List.nth all_p 0 in
                  
                  (* 2. 点炮者：当前回合的上家 *)
                  (* 因为打牌后回合已经切到了下家，所以 discarder 是 current - 1 *)
                  let curr_id = Game.current_player_id game in
                  let loser_id = (curr_id - 1 + 4) mod 4 in
                  let loser = List.nth all_p loser_id in

                  (* 3. 构造最终胡牌的手牌 (手牌 + 荣的那张) *)
                  (* let winning_hand = Hand.add (Player.hand winner) tile in *)

                   Dream.html (Printf.sprintf 
                      "<html>
                        <head>
                          <meta charset='utf-8'>
                          <title>荣和!</title>
                          <style>
                            body { background-color: #2d3436; color: white; font-family: 'Segoe UI', sans-serif; text-align: center; padding-top: 50px; }
                            .card { background: #333; display: inline-block; padding: 40px; border-radius: 15px; box-shadow: 0 10px 30px rgba(0,0,0,0.5); }
                            h1 { color: #fdcb6e; font-size: 4em; margin-bottom: 10px; text-shadow: 0 0 20px #e17055; }
                            h2 { color: #00b894; margin-top: 0; }
                            .loser { color: #ff7675; font-weight: bold; font-size: 1.2em; margin: 20px 0; }
                            a { color: #74b9ff; text-decoration: none; font-size: 1.2em; border: 1px solid #74b9ff; padding: 10px 30px; border-radius: 30px; transition: 0.3s; }
                            a:hover { background: #74b9ff; color: #2d3436; }
                          </style>
                        </head>
                        <body>
                         <div class='card'>
                           <h1>⚡ 荣和! Ron! ⚡</h1>
                           <h2>🎉 获胜者: %s</h2>
                           <div class='loser'>💥 点炮者: %s</div>
                           
                           <div style='margin: 30px 0; font-size: 1.5em; letter-spacing: 2px;'>
                             %s <span style='border: 2px solid #fdcb6e; padding: 2px 8px; border-radius: 4px; margin-left: 10px;'>%s</span>
                           </div>

                           <p style='margin-top: 50px;'><a href='/new_game'>再来一局</a></p>
                         </div>
                       </body></html>" 
                       (Player.name winner)
                       (Player.name loser)
                       (Hand.to_string (Player.hand winner)) (* 原有手牌 *)
                       (Tile.to_string tile) (* 荣的那张牌，高亮显示 *)
                   )
              | None -> Dream.redirect request "/")
         | None -> Dream.redirect request "/")
      | _ -> Dream.redirect request "/"
    );

(* 8. 自摸路由 (Tsumo) - 更新样式 *)
    Dream.post "/win" (fun _ ->
      let game = !game_state_ref in
      let p = Game.current_player game in
      if Player.can_tsumo p then
        Dream.html (Printf.sprintf 
          "<html>
            <body style='text-align: center; padding-top: 50px; background-color: #2d3436; color: white;'>
              <h1 style='color: #d63031; font-size: 3em;'>🎉 自摸！ 🎉</h1>
              <h2>获胜者: %s</h2>
              <div style='font-size: 2em; margin: 20px;'>%s</div>
              <p><a href='/new_game' style='color: #74b9ff'>再来一局</a></p>
            </body>
           </html>"
           (Player.name p)
           (Hand.to_string (Player.hand p))
        )
      else
        Dream.html "<h1>不可胡牌</h1><a href='/'>返回</a>"
    );

    Dream.get "/new_game" (fun request ->
      game_state_ref := Game.create ();
      Dream.redirect request "/"
    );
  ]