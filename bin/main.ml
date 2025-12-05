(* bin/main.ml *)
open Mahjong

let parse_tile_str s =
  let suits = [Tile.Man; Tile.Pin; Tile.Sou] in
  let nums = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let honors = [Tile.East; Tile.South; Tile.West; Tile.North; Tile.Red; Tile.Green; Tile.White] in
  
  let numbered = List.concat_map (fun s -> List.map (fun n -> Tile.Numbered (s, n)) nums) suits in
  let honored = List.map (fun h -> Tile.Honor h) honors in
  let all_unique_tiles = numbered @ honored in

  List.find_opt (fun t -> Tile.to_string t = s) all_unique_tiles

let game_state_ref = ref (Game.create ())

let render_melds (p: Player.t) =
  Player.melds p
  |> List.map (function
      | Player.Chi(a, b, c) ->
          Printf.sprintf "<div class='meld-box'><span class='tile static small'>%s</span><span class='tile static small'>%s</span><span class='tile static small active-meld'>%s</span></div>" (Tile.to_string a) (Tile.to_string b) (Tile.to_string c)
      | Player.Pon(a, _, _) ->
          Printf.sprintf "<div class='meld-box'><span class='tile static small'>%s</span><span class='tile static small'>%s</span><span class='tile static small active-meld'>%s</span></div>" (Tile.to_string a) (Tile.to_string a) (Tile.to_string a)
      | Player.Kan(a, _, _, _) ->
          Printf.sprintf "<div class='meld-box'><span class='tile static small'>%s</span><span class='tile static small'>%s</span><span class='tile static small'>%s</span><span class='tile static small active-meld'>%s</span></div>" (Tile.to_string a) (Tile.to_string a) (Tile.to_string a) (Tile.to_string a)
    )
  |> String.concat " "

let render_single_tile (tile: Tile.t) (idx: int) (is_clickable: bool) (is_new: bool) =
  let tile_str = Tile.to_string tile in
  let class_str = if is_clickable then "tile clickable" else "tile static" in
  let style_extra = if is_new then "margin-left: 20px; border: 2px solid #fdcb6e; transform: translateY(-8px);" else "" in
  
  if is_clickable then
    Printf.sprintf "<form action='/play' method='POST' style='display:inline-block; margin: 4px; %s'><input type='hidden' name='discard_index' value='%d'><button type='submit' class='%s'>%s</button></form>" style_extra idx class_str tile_str
  else
    Printf.sprintf "<div class='%s' style='%s'>%s</div>" class_str style_extra tile_str

let render_html (game: Game.t) : string =
  let all_players = Game.all_players game in
  let human_p = List.nth all_players 0 in 
  
  let current_idx = Game.current_player_id game in
  let is_my_turn = (current_idx = 0) in
  
  let can_discard = is_my_turn && Player.has_full_hand human_p in
  let can_draw = is_my_turn && not (Player.has_full_hand human_p) in

  let full_hand = Player.hand human_p in
  let drawn_opt = Player.last_drawn human_p in
  let last_discard_opt = Game.last_discard game in

  let suggestion_html =
    if can_discard then
      let recommendations = Player.get_recommendations human_p in
      let top3 = List.filteri (fun i _ -> i < 3) recommendations in
      
      let rows = 
        top3 |> List.map (fun (tile, count) ->
          Printf.sprintf 
            "<div style='display:flex; align-items:center; margin-bottom:8px; background:rgba(0,0,0,0.2); padding:5px; border-radius:4px;'>
               <span style='margin-right:10px; color:#a29bfe; font-weight:bold; font-size:0.9em;'>Discard:</span>
               <span class='tile static small' style='margin:0;'>%s</span>
               <div style='margin-left:auto; text-align:right;'>
                 <div style='color:#ccc; font-size:0.8em;'>Ukeire</div>
                 <div style='color:#fdcb6e; font-weight:bold; font-size:1.1em;'>%d</div>
               </div>
             </div>"
            (Tile.to_string tile) count
        ) |> String.concat ""
      in
      if rows = "" then "" 
      else 
        Printf.sprintf 
          "<div class='sidebar-panel'>
             <div class='sidebar-header'>💡 AI Assistant</div>
             %s
           </div>"
          rows
    else 
      "<div class='sidebar-panel' style='opacity:0.5'><div class='sidebar-header'>💡 AI Assistant</div><div style='padding:10px; font-size:0.9em; color:#888;'>Waiting for draw...</div></div>"
  in

  let action_buttons_html =
    match last_discard_opt with
    | None -> ""
    | Some target ->
        let t_str = Tile.to_string target in
        let btns = ref [] in
        let last_discarder = (current_idx - 1 + 4) mod 4 in

        if (not is_my_turn) && (last_discarder <> 0) then (
          if Player.can_ron human_p target then btns := (!btns) @ [Printf.sprintf "<form action='/win_ron' method='POST' style='display:inline'><input type='hidden' name='target' value='%s'><button class='win-btn'>⚡ Ron!</button></form>" t_str];
          if Player.can_pon human_p target then btns := (!btns) @ [Printf.sprintf "<form action='/pon' method='POST' style='display:inline; margin-left:10px;'><input type='hidden' name='target' value='%s'><button class='action-btn' style='background:#0984e3'>Pon</button></form>" t_str];
          if Player.can_kan human_p target then btns := (!btns) @ [Printf.sprintf "<form action='/kan' method='POST' style='display:inline; margin-left:10px;'><input type='hidden' name='target' value='%s'><button class='action-btn' style='background:#6c5ce7'>Kan</button></form>" t_str];
        );

        if is_my_turn && (not (Player.has_full_hand human_p)) then (
           let chi_opts = Player.find_chi_options human_p target in
           let chi_btns = chi_opts |> List.map (fun (t1, t2) -> Printf.sprintf "<form action='/chi' method='POST' style='display:inline; margin-left:10px;'><input type='hidden' name='target' value='%s'><input type='hidden' name='t1' value='%s'><input type='hidden' name='t2' value='%s'><button class='action-btn' style='background:#00b894'>Chi %s%s</button></form>" t_str (Tile.to_string t1) (Tile.to_string t2) (Tile.to_string t1) (Tile.to_string t2)) in
           btns := (!btns) @ chi_btns;
        );

        if !btns = [] then "" 
        else (
          let base_html = Printf.sprintf "<div class='action-panel'><h3>👇 Find: [%s]</h3>%s" t_str (String.concat "" !btns) in
          if not is_my_turn then base_html ^ "<form action='/bot_move' method='POST' style='display:inline; margin-left:20px;'><button class='action-btn' style='background:#b2bec3'>⏭ Pass</button></form></div>"
          else base_html ^ "</div>"
        )
  in

  let should_auto_play = (not is_my_turn) && (action_buttons_html = "") in
  let auto_play_script =
    if should_auto_play then
      "<script>setTimeout(function() { document.getElementById('bot-auto-form').submit(); }, 1000);</script>
       <form id='bot-auto-form' action='/bot_move' method='POST' style='display:none'></form>
       <div style='text-align:center; padding:10px; background:#ffeaa7; color:#d63031; font-weight:bold; border-radius:5px; margin-bottom:10px;'>⏳ Player " ^ (string_of_int current_idx) ^ " is thinking...</div>"
    else ""
  in

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

  let others_discards_html =
    [1; 2; 3] |> List.map (fun i ->
        if i < List.length all_players then
          let p = List.nth all_players i in
          let d_html = Player.discards p |> List.rev |> List.map (fun t -> Printf.sprintf "<span class='tile static small'>%s</span>" (Tile.to_string t)) |> String.concat "" in
          let content = if d_html = "" then "<span style='color:#636e72; font-size:0.8em'>No discards yet</span>" else d_html in
          let style = if current_idx = i then "border-left: 5px solid #00b894; background: #55efc410;" else "" in
          Printf.sprintf "<div class='other-row' style='%s'><div class='row-label'>%s</div><div class='row-tiles'>%s</div></div>" style (Player.name p) content
        else ""
      ) |> String.concat ""
  in

  let draw_button = if is_my_turn then (if can_draw then "<form action='/draw' method='POST'><button type='submit' class='action-btn draw-btn'>🖐 Draw</button></form>" else "<div class='info-msg'>Please discard a tile</div>") else "<div class='info-msg' style='color:#b2bec3'>Waiting for other players...</div>" in
  let tsumo_button = if is_my_turn && Player.can_tsumo human_p then "<form action='/win' method='POST'><button class='win-btn'>⚡ Tsumo!</button></form>" else "" in

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

  Printf.sprintf 
  "<html>
    <head>
      <title>OCaml Mahjong</title>
      <meta charset='utf-8'>
      <style>
        body { font-family: 'Segoe UI', sans-serif; padding: 20px; background-color: #2d3436; color: white; margin: 0; }
        
        .main-layout { display: flex; max-width: 1400px; margin: 0 auto; gap: 20px; align-items: flex-start; }
        .game-center { flex: 1; min-width: 0; }
        .sidebar { width: 240px; flex-shrink: 0; display: flex; flex-direction: column; gap: 20px; }

        .sidebar-panel { background: #353b48; border-radius: 8px; border: 1px solid #4b545f; padding: 10px; box-shadow: 0 4px 10px rgba(0,0,0,0.3); }
        .sidebar-header { color: #00b894; font-weight: bold; border-bottom: 1px solid #636e72; padding-bottom: 5px; margin-bottom: 10px; }

        .tile { width: 50px; height: 70px; border: 1px solid #999; border-radius: 6px; background: #fdfdfd; color: #333; font-weight: bold; font-size: 1.1em; display: flex; align-items: center; justify-content: center; box-shadow: 2px 2px 5px rgba(0,0,0,0.3); user-select: none; }
        .clickable { cursor: pointer; transition: transform 0.1s; border-bottom: 4px solid #b2bec3; } .clickable:hover { transform: translateY(-5px); border-bottom-color: #0984e3; color: #0984e3; }
        .static { display: inline-flex; margin: 4px; background: #e0e0e0; border-bottom: 4px solid #b2bec3; color: #636e72; }
        .small { width: 32px; height: 46px; font-size: 0.8em; margin: 2px; }
        .active-meld { border: 2px solid #fdcb6e; }
        .meld-box { display: inline-flex; margin-right: 15px; padding: 5px; background: #636e72; border-radius: 6px; }
        
        .hand-container { background: #353b48; padding: 20px; border-radius: 10px; min-height: 90px; display: flex; flex-wrap: wrap; align-items: center; border: 1px solid #4b545f; margin-bottom: 10px;}
        .other-discards-area { background: #353b48; padding: 15px; border-radius: 10px; margin-bottom: 20px; }
        .other-row { display: flex; align-items: center; border-bottom: 1px dashed #636e72; padding: 5px 0; }
        .row-label { width: 80px; font-weight: bold; color: #dfe6e9; font-size: 0.9em; }
        
        .action-panel { margin-bottom: 20px; padding: 15px; background: #fab1a0; border-radius: 5px; color: #2d3436; box-shadow: 0 0 15px #e17055; }
        .info-box { margin-bottom: 20px; padding: 10px; background: #dfe6e9; color: #2d3436; border-radius: 5px; display: flex; justify-content: space-between; }
        
        .discards { margin-top: 10px; font-family: monospace; color: #b2bec3; }
        .action-btn { font-size: 1em; padding: 8px 15px; border: none; border-radius: 5px; cursor: pointer; color: white; }
        .draw-btn { background-color: #00b894; font-size: 1.2em; padding: 10px 30px;}
        .win-btn { background-color: #d63031; color: white; font-size: 1.5em; padding: 15px 40px; border: none; border-radius: 50px; cursor: pointer; box-shadow: 0 4px 15px rgba(214, 48, 49, 0.5); animation: pulse 1.5s infinite; transition: transform 0.1s;} .win-btn:active {transform: scale(0.95);}
        .info-msg { color: #ffeaa7; font-weight: bold; font-size: 1.2em; }
        .debug-area { padding: 10px; background-color: #111; border: 1px solid #444; font-family: monospace; font-size: 0.8em; color: #aaa; border-radius: 8px; }
        @keyframes pulse { 0%% { transform: scale(1); } 50%% { transform: scale(1.05); } 100%% { transform: scale(1); } }
        a { color: #74b9ff; text-decoration: none; }
      </style>
    </head>
    <body>
      <h1 style='margin-left: 20px;'>🀄 OCaml Mahjong</h1>
      
      <div class='main-layout'>
        
        <div class='game-center'>
          %s <div class='info-box'>
            <div><div><strong>Current Turn:</strong> %s</div><div><strong>Remaining Tiles:</strong> %d</div></div>
            <div style='text-align: right'><div><strong>Your Melds:</strong> %s</div></div>
          </div>

          <div class='other-discards-area'>
             <div style='color: #b2bec3; margin-bottom: 10px; font-size: 0.9em;'>📺 Opponents' Status</div>
             %s
          </div>

          %s <div class='hand-container'>
            %s <div style='width: 30px;'></div> %s
          </div>
          
          <div style='margin-top: 30px; display: flex; gap: 20px; align-items: center;'>
            %s %s </div>

          <div class='discards'><strong>Your Discards:</strong> %s</div>
        </div>

        <div class='sidebar'>
           %s
           
           <div class='sidebar-panel'>
             <div class='sidebar-header'>🔧 Debug Info</div>
             <div style='font-size:0.8em; color:#aaa; overflow-x:hidden;'>
               %s
             </div>
           </div>
           
           <div style='text-align:center;'>
             <a href='/new_game'>⟳ Restart</a>
           </div>
        </div>

      </div>
    </body>
  </html>"
  auto_play_script
  (Player.name (List.nth all_players current_idx))
  (Game.remaining_tiles game)
  (render_melds human_p)
  others_discards_html
  action_buttons_html
  main_hand_html
  special_hand_html
  draw_button
  tsumo_button
  (Player.discards human_p |> List.rev |> List.map Tile.to_string |> String.concat " ")
  suggestion_html
  debug_html 

let () = 
  Printexc.record_backtrace true;
  print_endline "Server starting on http://localhost:3000 ...";
  
  Dream.run ~interface:"0.0.0.0" ~port:3000
  @@ Dream.logger
  @@ Dream.router [

    Dream.get "/" (fun _ -> 
      Dream.html (render_html !game_state_ref)
    );

    Dream.post "/draw" (fun req -> 
      let g = !game_state_ref in 
      let (ng, _) = Game.draw_card g in 
      game_state_ref := ng; 
      Dream.redirect req "/"
    );

    Dream.post "/play" (fun req -> 
      match%lwt Dream.form ~csrf:false req with 
      | `Ok [("discard_index", i)] -> 
          let idx = int_of_string i in 
          let g = !game_state_ref in 
          let p = Game.current_player g in 
          let hl = Player.hand p in 
          if idx < 1 || idx > List.length hl then 
            Dream.redirect req "/" 
          else 
            let t = List.nth hl (idx-1) in 
            let (ng1, _) = Game.discard_card g t in 
            game_state_ref := ng1; 
            Dream.redirect req "/" 
      | _ -> Dream.redirect req "/"
    );

    Dream.post "/bot_move" (fun req -> 
      let g = !game_state_ref in 
      if Game.current_player_id g <> 0 then (
        let (ng, _) = Game.play_bot_step g in 
        game_state_ref := ng; 
        Dream.redirect req "/"
      ) else (
        Dream.redirect req "/"
      )
    );

    Dream.post "/chi" (fun req -> 
      match%lwt Dream.form ~csrf:false req with 
      | `Ok form -> 
          let gf k = List.assoc_opt k form in 
          (match (gf "target", gf "t1", gf "t2") with 
           | (Some ts, Some t1s, Some t2s) -> 
               let target = parse_tile_str ts in 
               let t1 = parse_tile_str t1s in 
               let t2 = parse_tile_str t2s in 
               (match (target, t1, t2) with 
                | (Some t, Some a, Some b) -> 
                    let g = !game_state_ref in 
                    let (ng, success) = Game.perform_chi g t a b in 
                    if success then game_state_ref := ng; 
                    Dream.redirect req "/" 
                | _ -> Dream.redirect req "/")
           | _ -> Dream.redirect req "/")
      | _ -> Dream.redirect req "/"
    );

    Dream.post "/pon" (fun req -> 
      match%lwt Dream.form ~csrf:false req with 
      | `Ok form -> 
          (match List.assoc_opt "target" form with 
           | Some ts -> 
               (match parse_tile_str ts with 
                | Some t -> 
                    let g = !game_state_ref in 
                    let (ng, success) = Game.perform_pon g t in 
                    if success then game_state_ref := ng; 
                    Dream.redirect req "/" 
                | None -> Dream.redirect req "/")
           | None -> Dream.redirect req "/")
      | _ -> Dream.redirect req "/"
    );

    Dream.post "/kan" (fun req -> 
      match%lwt Dream.form ~csrf:false req with 
      | `Ok form -> 
          (match List.assoc_opt "target" form with 
           | Some ts -> 
               (match parse_tile_str ts with 
                | Some t -> 
                    let g = !game_state_ref in 
                    let (ng, success) = Game.perform_kan g t in 
                    if success then game_state_ref := ng; 
                    Dream.redirect req "/" 
                | None -> Dream.redirect req "/")
           | None -> Dream.redirect req "/")
      | _ -> Dream.redirect req "/"
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
                  let winner = List.nth all_p 0 in
                  let curr_id = Game.current_player_id game in
                  let loser_id = (curr_id - 1 + 4) mod 4 in
                  let loser = List.nth all_p loser_id in

                  Dream.html (Printf.sprintf 
                      "<html><head><meta charset='utf-8'><title>Ron!</title><style>body { background-color: #2d3436; color: white; font-family: 'Segoe UI', sans-serif; text-align: center; padding-top: 50px; } .card { background: #333; display: inline-block; padding: 40px; border-radius: 15px; box-shadow: 0 10px 30px rgba(0,0,0,0.5); } h1 { color: #fdcb6e; font-size: 4em; margin-bottom: 10px; text-shadow: 0 0 20px #e17055; } h2 { color: #00b894; margin-top: 0; } .loser { color: #ff7675; font-weight: bold; font-size: 1.2em; margin: 20px 0; } a { color: #74b9ff; text-decoration: none; font-size: 1.2em; border: 1px solid #74b9ff; padding: 10px 30px; border-radius: 30px; transition: 0.3s; } a:hover { background: #74b9ff; color: #2d3436; } </style></head><body><div class='card'><h1>⚡ Ron! ⚡</h1><h2>🎉 Winner: %s</h2><div class='loser'>💥 Loser: %s</div><div style='margin: 30px 0; font-size: 1.5em; letter-spacing: 2px;'>%s <span style='border: 2px solid #fdcb6e; padding: 2px 8px; border-radius: 4px; margin-left: 10px;'>%s</span></div><p style='margin-top: 50px;'><a href='/new_game'>Play Again</a></p></div></body></html>" 
                       (Player.name winner) (Player.name loser) (Hand.to_string (Player.hand winner)) (Tile.to_string tile))
              | None -> Dream.redirect request "/")
         | None -> Dream.redirect request "/")
      | _ -> Dream.redirect request "/"
    );

    Dream.post "/win" (fun _ ->
      let game = !game_state_ref in
      let all_p = Game.all_players game in
      let p0 = List.nth all_p 0 in
      if Player.can_tsumo p0 then
        Dream.html (Printf.sprintf 
          "<html><body style='text-align: center; padding-top: 50px; background-color: #2d3436; color: white;'><h1 style='color: #d63031; font-size: 3em;'>🎉 役满！自摸！ 🎉</h1><h2>获胜者: %s</h2><div style='font-size: 2em; margin: 20px;'>%s</div><p><a href='/new_game' style='color: #74b9ff'>再来一局</a></p></body></html>"
           (Player.name p0) (Hand.to_string (Player.hand p0)))
      else
        Dream.html "<h1>Cannot Win</h1><a href='/'>Back</a>"
    );

    Dream.get "/new_game" (fun req -> 
      game_state_ref := Game.create (); 
      Dream.redirect req "/"
    );
  ]