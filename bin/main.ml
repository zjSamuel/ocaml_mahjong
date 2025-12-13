open Mahjong


(* 1. Helpers & Global State *)

(* Helper to convert string representations back to internal Tile types. *)
let parse_tile_str s =
  let suits = [ Tile.Man; Tile.Pin; Tile.Sou ] in
  let nums = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  let honors =
    [
      Tile.East;
      Tile.South;
      Tile.West;
      Tile.North;
      Tile.Red;
      Tile.Green;
      Tile.White;
    ]
  in

  let numbered =
    List.concat_map
      (fun s -> List.map (fun n -> Tile.Numbered (s, n)) nums)
      suits
  in
  let honored = List.map (fun h -> Tile.Honor h) honors in
  let all_unique_tiles = numbered @ honored in

  List.find_opt (fun t -> Tile.to_string t = s) all_unique_tiles

let game_state_ref = ref (Game.create ())


(* 2. View Layer: UI Components, use server-side render*)

(* difficulty badge (Easy/Medium/Hard) *)
let render_difficulty_badge (diff : Player.difficulty) =
  match diff with
  | Player.Easy ->
      "<span style='background:#00b894; color:white; padding:2px 6px; \
       border-radius:4px; font-size:0.7em; vertical-align:middle;'>Easy</span>"
  | Player.Medium ->
      "<span style='background:#fdcb6e; color:#2d3436; padding:2px 6px; \
       border-radius:4px; font-size:0.7em; \
       vertical-align:middle;'>Medium</span>"
  | Player.Hard ->
      "<span style='background:#d63031; color:white; padding:2px 6px; \
       border-radius:4px; font-size:0.7em; vertical-align:middle;'>Hard</span>"

(* the control in the sidebar to switch bot difficulty *)
let render_difficulty_controls (game : Game.t) =
  let players = Game.all_players game in
  let rows =
    players
    |> List.mapi (fun i p ->
           if i = 0 then ""
           else
             let diff = Player.difficulty p in
             let btn_style current target color =
               if current = target then
                 Printf.sprintf
                   "background:%s; color:white; border:1px solid %s;" color
                   color
               else
                 "background:transparent; color:#b2bec3; border:1px solid \
                  #636e72;"
             in
             Printf.sprintf
               "<div style='display:flex; justify-content:space-between; \
                align-items:center; margin-bottom:8px;'>\n\
               \             <div style='font-size:0.9em;'>%s</div>\n\
               \             <div>\n\
               \               <a href='/set_diff?p=%d&d=easy' \
                style='text-decoration:none; padding:2px 5px; font-size:0.7em; \
                border-radius:3px; margin-right:2px; %s'>E</a>\n\
               \               <a href='/set_diff?p=%d&d=medium' \
                style='text-decoration:none; padding:2px 5px; font-size:0.7em; \
                border-radius:3px; margin-right:2px; %s'>M</a>\n\
               \               <a href='/set_diff?p=%d&d=hard' \
                style='text-decoration:none; padding:2px 5px; font-size:0.7em; \
                border-radius:3px; %s'>H</a>\n\
               \             </div>\n\
               \           </div>"
               (Player.name p) i
               (btn_style diff Player.Easy "#00b894")
               i
               (btn_style diff Player.Medium "#fdcb6e")
               i
               (btn_style diff Player.Hard "#d63031"))
    |> String.concat ""
  in
  Printf.sprintf
    "<div class='sidebar-panel'>\n\
    \       <div class='sidebar-header'>🤖 Bot Intelligence</div>\n\
    \       %s\n\
    \     </div>"
    rows



(* render the Yaku list and total score *)
let render_score_result (res : Hand.Score.result) =
  let yaku_rows =
    res.yaku_list
    |> List.map (function
         | Hand.Score.MenzenTsumo -> "门前清自摸和 (1番)"
         | Hand.Score.Riichi -> "立直 (1番)"
         | Hand.Score.Ippatsu -> "一发 (1番)"
         | Hand.Score.Pinfu -> "平和 (1番)"
         | Hand.Score.Tanyao -> "断幺九 (1番)"
         | Hand.Score.Iipeiko -> "一盃口 (1番)"
         | Hand.Score.Yakuhai s -> Printf.sprintf "役牌: %s (1番)" s
         | Hand.Score.Rinshan -> "岭上开花 (1番)"
         | Hand.Score.Sanshoku -> "三色同顺 (2番/1番)"
         | Hand.Score.Itsu -> "一气通贯 (2番/1番)"
         | Hand.Score.Chanta -> "混全带幺九 (2番/1番)"
         | Hand.Score.Chiitoitsu -> "七对子 (2番)"
         | Hand.Score.Toitoi -> "对对和 (2番)"
         | Hand.Score.Sanankou -> "三暗刻 (2番)"
         | Hand.Score.Sankantsu -> "三杠子 (2番)"
         | Hand.Score.SanshokuDoukou -> "三色同刻 (2番)"
         | Hand.Score.Honroutou -> "混老头 (2番)"
         | Hand.Score.Shousangen -> "小三元 (2番)"
         | Hand.Score.Honitsu -> "混一色 (3番/2番)"
         | Hand.Score.Junchan -> "纯全带幺九 (3番/2番)"
         | Hand.Score.Ryanpeiko -> "二盃口 (3番)"
         | Hand.Score.Chinitsu -> "清一色 (6番/5番)"
         | Hand.Score.Dora n -> Printf.sprintf "宝牌 (%d番)" n)
    |> List.map (fun s ->
           Printf.sprintf
             "<div style='margin: 4px 0; border-bottom: 1px dashed #555; \
              padding-bottom: 2px;'>%s</div>"
             s)
    |> String.concat ""
  in
  Printf.sprintf
    "<div style='background:#2d3436; padding:20px; border-radius:10px; \
     margin:20px auto; width:320px; text-align:left; border: 1px solid \
     #fdcb6e; box-shadow: 0 5px 15px rgba(0,0,0,0.5);'>\n\
    \       <h3 style='color:#fdcb6e; border-bottom:1px solid #636e72; \
     padding-bottom:10px; margin-top:0; display:flex; \
     justify-content:space-between;'>\n\
    \         <span>Total Han:</span> <span style='font-size:1.2em;'>%d</span>\n\
    \       </h3>\n\
    \       <div style='color: #dfe6e9; font-size: 0.95em;'>%s</div>\n\
    \       <div style='margin-top: 15px; text-align: right; color: #b2bec3; \
     font-size: 0.8em;'>Fu: %d</div>\n\
    \     </div>"
    res.han yaku_rows res.fu

(* open melds (Chi/Pon/Kan) for player. *)
let render_melds (p : Player.t) =
  Player.melds p
  |> List.map (function
       | Hand.Chi (a, b, c) ->
           Printf.sprintf
             "<div class='meld-box'><span class='tile static \
              small'>%s</span><span class='tile static small'>%s</span><span \
              class='tile static small active-meld'>%s</span></div>"
             (Tile.to_string a) (Tile.to_string b) (Tile.to_string c)
       | Hand.Pon (a, _, _) ->
           Printf.sprintf
             "<div class='meld-box'><span class='tile static \
              small'>%s</span><span class='tile static small'>%s</span><span \
              class='tile static small active-meld'>%s</span></div>"
             (Tile.to_string a) (Tile.to_string a) (Tile.to_string a)
       | Hand.Kan (a, _, _, _) ->
           Printf.sprintf
             "<div class='meld-box'><span class='tile static \
              small'>%s</span><span class='tile static small'>%s</span><span \
              class='tile static small'>%s</span><span class='tile static \
              small active-meld'>%s</span></div>"
             (Tile.to_string a) (Tile.to_string a) (Tile.to_string a)
             (Tile.to_string a))
  |> String.concat " "

let render_single_tile (tile : Tile.t) (idx : int) (is_clickable : bool)
    (is_new : bool) =
  let tile_str = Tile.to_string tile in
  let class_str = if is_clickable then "tile clickable" else "tile static" in
  let style_extra =
    if is_new then
      "margin-left: 20px; border: 2px solid #fdcb6e; transform: \
       translateY(-8px);"
    else ""
  in

  if is_clickable then
    Printf.sprintf
      "<form action='/play' method='POST' style='display:inline-block; margin: \
       4px; %s'><input type='hidden' name='discard_index' value='%d'><button \
       type='submit' class='%s'>%s</button></form>"
      style_extra idx class_str tile_str
  else
    Printf.sprintf "<div class='%s' style='%s'>%s</div>" class_str style_extra
      tile_str


(* 3. Main Render *)

(* Generate the full HTML page for the current game state. *)
let render_html (game : Game.t) : string =
  let all_players = Game.all_players game in
  let human_p = List.nth all_players 0 in

  let current_idx = Game.current_player_id game in
  let is_my_turn = current_idx = 0 in

  (* determine available actions for the human player *)
  let can_discard = is_my_turn && Player.has_full_hand human_p in
  let can_draw = is_my_turn && not (Player.has_full_hand human_p) in

  let full_hand = Player.hand human_p in
  let drawn_opt = Player.last_drawn human_p in
  let last_discard_opt = Game.last_discard game in

  (* AI Assistant, If it's the player's discard turn, call the backend A* algorithm 
     to render discard recommendations and Ukeire stats.*)
  let suggestion_html =
    if can_discard then
      let visible = Game.get_visible_counts game 0 in
      let indicators = Game.get_dora_indicators game in

      let recommendations =
        Player.get_recommendations_enhanced human_p visible indicators
      in
      let top3 = List.filteri (fun i _ -> i < 3) recommendations in

      let rows =
        top3
        |> List.map (fun (tile, count, score) ->
               Printf.sprintf
                 "<div style='display:flex; align-items:center; \
                  margin-bottom:8px; background:rgba(0,0,0,0.2); padding:5px; \
                  border-radius:4px;'>\n\
                 \               <span style='margin-right:10px; \
                  color:#a29bfe; font-weight:bold; \
                  font-size:0.9em;'>Discard:</span>\n\
                 \               <span class='tile static small' \
                  style='margin:0;'>%s</span>\n\
                 \               <div style='margin-left:auto; \
                  text-align:right;'>\n\
                 \                 <div style='color:#ccc; \
                  font-size:0.8em;'>Ukeire: <span \
                  style='color:#fff'>%d</span></div>\n\
                 \                 <div style='color:#fdcb6e; \
                  font-weight:bold; font-size:0.9em;'>Score: %.1f</div>\n\
                 \               </div>\n\
                 \             </div>"
                 (Tile.to_string tile) count score)
        |> String.concat ""
      in
      if rows = "" then ""
      else
        Printf.sprintf
          "<div class='sidebar-panel'>\n\
          \             <div class='sidebar-header'>💡 AI Assistant \
           (Enhanced)</div>\n\
          \             %s\n\
          \           </div>"
          rows
    else
      "<div class='sidebar-panel' style='opacity:0.5'><div \
       class='sidebar-header'>💡 AI Assistant</div><div style='padding:10px; \
       font-size:0.9em; color:#888;'>Waiting for draw...</div></div>"
  in

  (* Dynamic Action Buttons (Chi/Pon/Kan/Ron) *)
  (* check if user can interrupt with Pon/Kan/Ron based on the last discard. *)
  let action_buttons_html =
    match last_discard_opt with
    | None -> ""
    | Some target ->
        let t_str = Tile.to_string target in
        let btns = ref [] in
        let last_discarder = (current_idx - 1 + 4) mod 4 in

        if (not is_my_turn) && last_discarder <> 0 then (
          if Player.can_ron human_p target then
            btns :=
              !btns
              @ [
                  Printf.sprintf
                    "<form action='/win_ron' method='POST' \
                     style='display:inline'><input type='hidden' name='target' \
                     value='%s'><button class='win-btn'>⚡ Ron!</button></form>"
                    t_str;
                ];
          if Player.can_pon human_p target then
            btns :=
              !btns
              @ [
                  Printf.sprintf
                    "<form action='/pon' method='POST' style='display:inline; \
                     margin-left:10px;'><input type='hidden' name='target' \
                     value='%s'><button class='action-btn' \
                     style='background:#0984e3'>Pon</button></form>"
                    t_str;
                ];
          if Player.can_kan human_p target then
            btns :=
              !btns
              @ [
                  Printf.sprintf
                    "<form action='/kan' method='POST' style='display:inline; \
                     margin-left:10px;'><input type='hidden' name='target' \
                     value='%s'><button class='action-btn' \
                     style='background:#6c5ce7'>Kan</button></form>"
                    t_str;
                ]);

        (if is_my_turn && not (Player.has_full_hand human_p) then
           let chi_opts = Player.find_chi_options human_p target in
           let chi_btns =
             chi_opts
             |> List.map (fun (t1, t2) ->
                    Printf.sprintf
                      "<form action='/chi' method='POST' \
                       style='display:inline; margin-left:10px;'><input \
                       type='hidden' name='target' value='%s'><input \
                       type='hidden' name='t1' value='%s'><input type='hidden' \
                       name='t2' value='%s'><button class='action-btn' \
                       style='background:#00b894'>Chi %s%s</button></form>"
                      t_str (Tile.to_string t1) (Tile.to_string t2)
                      (Tile.to_string t1) (Tile.to_string t2))
           in
           btns := !btns @ chi_btns);

        if !btns = [] then ""
        else
          let base_html =
            Printf.sprintf "<div class='action-panel'><h3>👇 Find: [%s]</h3>%s"
              t_str (String.concat "" !btns)
          in
          if not is_my_turn then
            base_html
            ^ "<form action='/bot_move' method='POST' style='display:inline; \
               margin-left:20px;'><button class='action-btn' \
               style='background:#b2bec3'>⏭ Pass</button></form></div>"
          else base_html ^ "</div>"
  in

  (* Game loop auto part  *)
  (* If not the human's turn and no actions are required, auto run bot *)
  let should_auto_play = (not is_my_turn) && action_buttons_html = "" in
  let auto_play_script =
    if should_auto_play then
      "<script>setTimeout(function() { \
       document.getElementById('bot-auto-form').submit(); }, 1000);</script>\n\
      \       <form id='bot-auto-form' action='/bot_move' method='POST' \
       style='display:none'></form>\n\
      \       <div style='text-align:center; padding:10px; background:#ffeaa7; \
       color:#d63031; font-weight:bold; border-radius:5px; \
       margin-bottom:10px;'>⏳ Player " ^ string_of_int current_idx
      ^ " is thinking...</div>"
    else ""
  in

  let indexed_hand = List.mapi (fun i t -> (t, i + 1)) full_hand in
  let main_part, special_part =
    match (can_discard, drawn_opt) with
    | true, Some new_tile -> (
        let matches =
          List.filter (fun (t, _) -> Tile.compare t new_tile = 0) indexed_hand
        in
        match List.rev matches with
        | (t, idx) :: _ ->
            (List.filter (fun (_, i) -> i <> idx) indexed_hand, Some (t, idx))
        | [] -> (indexed_hand, None))
    | _ -> (indexed_hand, None)
  in
  let main_hand_html =
    main_part
    |> List.map (fun (t, idx) -> render_single_tile t idx can_discard false)
    |> String.concat ""
  in
  let special_hand_html =
    match special_part with
    | Some (t, idx) -> render_single_tile t idx can_discard true
    | None -> ""
  in

  let others_discards_html =
    [ 1; 2; 3 ]
    |> List.map (fun i ->
           if i < List.length all_players then
             let p = List.nth all_players i in
             let name_html =
               Printf.sprintf "%s %s" (Player.name p)
                 (render_difficulty_badge (Player.difficulty p))
             in

             let d_html =
               Player.discards p |> List.rev
               |> List.map (fun t ->
                      Printf.sprintf "<span class='tile static small'>%s</span>"
                        (Tile.to_string t))
               |> String.concat ""
             in
             let content =
               if d_html = "" then
                 "<span style='color:#636e72; font-size:0.8em'>No discards \
                  yet</span>"
               else d_html
             in
             let style =
               if current_idx = i then
                 "border-left: 5px solid #00b894; background: #55efc410;"
               else ""
             in
             Printf.sprintf
               "<div class='other-row' style='%s'><div \
                class='row-label'>%s</div><div \
                class='row-tiles'>%s</div></div>"
               style name_html content
           else "")
    |> String.concat ""
  in

  let draw_button =
    if is_my_turn then
      if can_draw then
        "<form action='/draw' method='POST'><button type='submit' \
         class='action-btn draw-btn'>🖐 Draw</button></form>"
      else "<div class='info-msg'>Please discard a tile</div>"
    else
      "<div class='info-msg' style='color:#b2bec3'>Waiting for other \
       players...</div>"
  in
  let tsumo_button =
    if is_my_turn && Player.can_tsumo human_p then
      "<form action='/win' method='POST'><button class='win-btn'>⚡ \
       Tsumo!</button></form>"
    else ""
  in

  let indicators = Game.get_dora_indicators game in
  let dora_html =
    indicators
    |> List.map (fun t ->
           Printf.sprintf
             "<span class='tile static small' \
              style='border-color:#fdcb6e;'>%s</span>"
             (Tile.to_string t))
    |> String.concat ""
  in

  let debug_html =
    Game.all_players game
    |> List.mapi (fun i p ->
           let is_acting = i = current_idx in
           let style =
             if is_acting then "color: #00b894; font-weight:bold;"
             else "color: #b2bec3;"
           in
           Printf.sprintf
             "<div style='margin-bottom:5px; border-bottom:1px dashed \
              #444;'><span style='%s'>%s %s</span><span \
              style='margin-left:10px'>✋%s</span> <span \
              style='margin-left:10px'>🧊%s</span></div>"
             style (Player.name p)
             (if is_acting then "(Thinking)" else "")
             (Hand.to_string (Player.hand p))
             (String.concat " " (List.map (fun _ -> "Meld") (Player.melds p))))
    |> String.concat "\n"
  in
  let diff_controls = render_difficulty_controls game in

  Printf.sprintf
    "<html>\n\
    \    <head>\n\
    \      <title>OCaml Mahjong</title>\n\
    \      <meta charset='utf-8'>\n\
    \      <style>\n\
    \        body { font-family: 'Segoe UI', sans-serif; padding: 20px; \
     background-color: #2d3436; color: white; margin: 0; }\n\
    \        \n\
    \        .main-layout { display: flex; max-width: 1400px; margin: 0 auto; \
     gap: 20px; align-items: flex-start; }\n\
    \        .game-center { flex: 1; min-width: 0; }\n\
    \        .sidebar { width: 240px; flex-shrink: 0; display: flex; \
     flex-direction: column; gap: 20px; }\n\n\
    \        .sidebar-panel { background: #353b48; border-radius: 8px; border: \
     1px solid #4b545f; padding: 10px; box-shadow: 0 4px 10px rgba(0,0,0,0.3); \
     }\n\
    \        .sidebar-header { color: #00b894; font-weight: bold; \
     border-bottom: 1px solid #636e72; padding-bottom: 5px; margin-bottom: \
     10px; }\n\n\
    \        .tile { width: 50px; height: 70px; border: 1px solid #999; \
     border-radius: 6px; background: #fdfdfd; color: #333; font-weight: bold; \
     font-size: 1.1em; display: flex; align-items: center; justify-content: \
     center; box-shadow: 2px 2px 5px rgba(0,0,0,0.3); user-select: none; }\n\
    \        .clickable { cursor: pointer; transition: transform 0.1s; \
     border-bottom: 4px solid #b2bec3; } .clickable:hover { transform: \
     translateY(-5px); border-bottom-color: #0984e3; color: #0984e3; }\n\
    \        .static { display: inline-flex; margin: 4px; background: #e0e0e0; \
     border-bottom: 4px solid #b2bec3; color: #636e72; }\n\
    \        .small { width: 32px; height: 46px; font-size: 0.8em; margin: \
     2px; }\n\
    \        .active-meld { border: 2px solid #fdcb6e; }\n\
    \        .meld-box { display: inline-flex; margin-right: 15px; padding: \
     5px; background: #636e72; border-radius: 6px; }\n\
    \        \n\
    \        .hand-container { background: #353b48; padding: 20px; \
     border-radius: 10px; min-height: 90px; display: flex; flex-wrap: wrap; \
     align-items: center; border: 1px solid #4b545f; margin-bottom: 10px;}\n\
    \        .other-discards-area { background: #353b48; padding: 15px; \
     border-radius: 10px; margin-bottom: 20px; }\n\
    \        .other-row { display: flex; align-items: center; border-bottom: \
     1px dashed #636e72; padding: 5px 0; }\n\
    \        .row-label { width: 80px; font-weight: bold; color: #dfe6e9; \
     font-size: 0.9em; }\n\
    \        \n\
    \        .action-panel { margin-bottom: 20px; padding: 15px; background: \
     #fab1a0; border-radius: 5px; color: #2d3436; box-shadow: 0 0 15px \
     #e17055; }\n\
    \        .info-box { margin-bottom: 20px; padding: 10px; background: \
     #dfe6e9; color: #2d3436; border-radius: 5px; display: flex; \
     justify-content: space-between; }\n\
    \        \n\
    \        .discards { margin-top: 10px; font-family: monospace; color: \
     #b2bec3; }\n\
    \        .action-btn { font-size: 1em; padding: 8px 15px; border: none; \
     border-radius: 5px; cursor: pointer; color: white; }\n\
    \        .draw-btn { background-color: #00b894; font-size: 1.2em; padding: \
     10px 30px;}\n\
    \        .win-btn { background-color: #d63031; color: white; font-size: \
     1.5em; padding: 15px 40px; border: none; border-radius: 50px; cursor: \
     pointer; box-shadow: 0 4px 15px rgba(214, 48, 49, 0.5); animation: pulse \
     1.5s infinite; transition: transform 0.1s;} .win-btn:active {transform: \
     scale(0.95);}\n\
    \        .info-msg { color: #ffeaa7; font-weight: bold; font-size: 1.2em; }\n\
    \        .debug-area { padding: 10px; background-color: #111; border: 1px \
     solid #444; font-family: monospace; font-size: 0.8em; color: #aaa; \
     border-radius: 8px; }\n\
    \        @keyframes pulse { 0%% { transform: scale(1); } 50%% { transform: \
     scale(1.05); } 100%% { transform: scale(1); } }\n\
    \        a { color: #74b9ff; text-decoration: none; }\n\
    \      </style>\n\
    \    </head>\n\
    \    <body>\n\
    \      <h1 style='margin-left: 20px;'>🀄 OCaml Mahjong</h1>\n\
    \      \n\
    \      <div class='main-layout'>\n\
    \        \n\
    \        <div class='game-center'>\n\
    \          %s <div class='info-box'>\n\
    \            <div><div><strong>Current Turn:</strong> \
     %s</div><div><strong>Remaining Tiles:</strong> %d</div><div \
     style='display:flex; align-items:center;'><strong>Dora:</strong> <div \
     style='display:inline-block; margin-left:10px;'>%s</div></div></div>\n\
    \            <div style='text-align: right'><div><strong>Your \
     Melds:</strong> %s</div></div>\n\
    \          </div>\n\n\
    \          <div class='other-discards-area'>\n\
    \             <div style='color: #b2bec3; margin-bottom: 10px; font-size: \
     0.9em;'>📺 Opponents' Status</div>\n\
    \             %s\n\
    \          </div>\n\n\
    \          %s <div class='hand-container'>\n\
    \            %s <div style='width: 30px;'></div> %s\n\
    \          </div>\n\
    \          \n\
    \          <div style='margin-top: 30px; display: flex; gap: 20px; \
     align-items: center;'>\n\
    \            %s %s </div>\n\n\
    \          <div class='discards'><strong>Your Discards:</strong> %s</div>\n\
    \        </div>\n\n\
    \        <div class='sidebar'>\n\
    \           %s\n\
    \           %s\n\
    \           \n\
    \           <div class='sidebar-panel'>\n\
    \             <div class='sidebar-header'>🔧 Debug Info</div>\n\
    \             <div style='font-size:0.8em; color:#aaa; overflow-x:hidden;'>\n\
    \               %s\n\
    \             </div>\n\
    \           </div>\n\
    \           \n\
    \           <div style='text-align:center;'>\n\
    \             <a href='/new_game'>⟳ Restart</a>\n\
    \           </div>\n\
    \        </div>\n\n\
    \      </div>\n\
    \    </body>\n\
    \  </html>"
    auto_play_script
    (Player.name (List.nth all_players current_idx))
    (Game.remaining_tiles game)
    dora_html (render_melds human_p) others_discards_html action_buttons_html
    main_hand_html special_hand_html draw_button tsumo_button
    (Player.discards human_p |> List.rev |> List.map Tile.to_string
   |> String.concat " ")
    suggestion_html diff_controls debug_html


(* 4. Router and Controller *)
let () =
  Printexc.record_backtrace true;
  print_endline "Server starting on http://localhost:3000 ...";

  Dream.run ~interface:"0.0.0.0" ~port:3000
  @@ Dream.logger
  @@ Dream.router
       [
        (* GET / : Main entry point, renders the game UI. *)
         Dream.get "/" (fun _ -> Dream.html (render_html !game_state_ref));
         (* GET /set_diff : Controller to update bot difficulty settings. *)
         Dream.get "/set_diff" (fun req ->
             let p_idx_opt = Dream.query req "p" in
             let diff_str_opt = Dream.query req "d" in
             match (p_idx_opt, diff_str_opt) with
             | Some p_s, Some d_s ->
                 let p_idx = int_of_string p_s in
                 let diff =
                   match d_s with
                   | "easy" -> Player.Easy
                   | "hard" -> Player.Hard
                   | _ -> Player.Medium
                 in
                 let g = !game_state_ref in
                 game_state_ref := Game.set_bot_difficulty g p_idx diff;
                 Dream.redirect req "/"
             | _ -> Dream.redirect req "/");
        (* POST /draw : Handles the player's draw action. *)
         Dream.post "/draw" (fun req ->
             let g = !game_state_ref in
             let ng, _ = Game.draw_card g in
             game_state_ref := ng;
             Dream.redirect req "/"); (* redirect to refresh view *)
        (* POST /play : Parses the 'discard_index' from the form data. *)
         Dream.post "/play" (fun req ->
             match%lwt Dream.form ~csrf:false req with
             | `Ok [ ("discard_index", i) ] ->
                 let idx = int_of_string i in
                 let g = !game_state_ref in
                 let p = Game.current_player g in
                 let hl = Player.hand p in
                 if idx < 1 || idx > List.length hl then Dream.redirect req "/"
                 else
                   let t = List.nth hl (idx - 1) in
                   let ng1, _ = Game.discard_card g t in
                   game_state_ref := ng1;
                   Dream.redirect req "/"
             | _ -> Dream.redirect req "/");
        (* POST /bot_move : Executes one step of AI logic and updates the state. *)
         Dream.post "/bot_move" (fun req ->
             let g = !game_state_ref in
             if Game.current_player_id g <> 0 then (
               let ng, _ = Game.play_bot_step g in
               game_state_ref := ng;
              
              (* Check for AI victory after the move *)
               match Game.winner ng with
               (* Render the victory screen if AI wins *)
               | Some winner_p ->
                   let indicators = Game.get_dora_indicators ng in
                   let score_res =
                     Hand.calculate_score (Player.hand winner_p)
                       (Player.melds winner_p) indicators Tile.East Tile.East
                       true false
                   in

                   let score_html =
                     match score_res with
                     | Some res -> render_score_result res
                     | None -> "<div style='color:red'>役なし (No Yaku)</div>"
                   in

                   Dream.html
                     (Printf.sprintf
                        "<html><head><meta charset='utf-8'><title>Bot \
                         Win!</title><style>body { background-color: #2d3436; \
                         color: white; font-family: sans-serif; text-align: \
                         center; padding-top: 50px; } .card { background: \
                         #333; display: inline-block; padding: 40px; \
                         border-radius: 15px; box-shadow: 0 10px 30px \
                         rgba(0,0,0,0.5); } h1 { color: #d63031; font-size: \
                         3em; } a { color: #74b9ff; font-size: 1.2em; \
                         text-decoration: none; border: 1px solid #74b9ff; \
                         padding: 10px 30px; border-radius: 30px; } a:hover { \
                         background: #74b9ff; color: #2d3436; \
                         }</style></head><body><div class='card'><h1>🤖 Bot \
                         Tsumo!</h1><h2>Winner: %s</h2><div style='font-size: \
                         2em; margin: 30px 0;'>%s</div>%s<p style='margin-top: \
                         50px;'><a href='/new_game'>Play \
                         Again</a></p></div></body></html>"
                        (Player.name winner_p)
                        (Hand.to_string (Player.hand winner_p))
                        score_html)
               | None ->
                   Dream.redirect req "/")
             else Dream.redirect req "/");
         (* POST /chi : controllers for meld action. *)
         Dream.post "/chi" (fun req ->
             match%lwt Dream.form ~csrf:false req with
             | `Ok form -> (
                 let gf k = List.assoc_opt k form in
                 match (gf "target", gf "t1", gf "t2") with
                 | Some ts, Some t1s, Some t2s -> (
                     let target = parse_tile_str ts in
                     let t1 = parse_tile_str t1s in
                     let t2 = parse_tile_str t2s in
                     match (target, t1, t2) with
                     | Some t, Some a, Some b ->
                         let g = !game_state_ref in
                         let ng, success = Game.perform_chi g t a b in
                         if success then game_state_ref := ng;
                         Dream.redirect req "/"
                     | _ -> Dream.redirect req "/")
                 | _ -> Dream.redirect req "/")
             | _ -> Dream.redirect req "/");
         (* POST /pon : Controller for meld action *)
         Dream.post "/pon" (fun req ->
             match%lwt Dream.form ~csrf:false req with
             | `Ok form -> (
                 match List.assoc_opt "target" form with
                 | Some ts -> (
                     match parse_tile_str ts with
                     | Some t ->
                         let g = !game_state_ref in
                         let ng, success = Game.perform_pon g t in
                         if success then game_state_ref := ng;
                         Dream.redirect req "/"
                     | None -> Dream.redirect req "/")
                 | None -> Dream.redirect req "/")
             | _ -> Dream.redirect req "/");
         (* POST /kan : Controller for meld action *)
         Dream.post "/kan" (fun req ->
             match%lwt Dream.form ~csrf:false req with
             | `Ok form -> (
                 match List.assoc_opt "target" form with
                 | Some ts -> (
                     match parse_tile_str ts with
                     | Some t ->
                         let g = !game_state_ref in
                         let ng, success = Game.perform_kan g t in
                         if success then game_state_ref := ng;
                         Dream.redirect req "/"
                     | None -> Dream.redirect req "/")
                 | None -> Dream.redirect req "/")
             | _ -> Dream.redirect req "/");
         (* POST /win_ron : Handles the specific win when player calls Ron. *)
         Dream.post "/win_ron" (fun request ->
             match%lwt Dream.form ~csrf:false request with
             | `Ok form -> (
                 match List.assoc_opt "target" form with
                 | Some ts -> (
                     match parse_tile_str ts with
                     | Some tile ->
                         let game = !game_state_ref in
                         let all_p = Game.all_players game in
                         let winner = List.nth all_p 0 in
                         let curr_id = Game.current_player_id game in
                         let loser_id = (curr_id - 1 + 4) mod 4 in
                         let loser = List.nth all_p loser_id in

                         let full_hand = Hand.add (Player.hand winner) tile in
                         let indicators = Game.get_dora_indicators game in

                         let score_html =
                           match
                             Hand.calculate_score full_hand
                               (Player.melds winner) indicators Tile.East
                               Tile.East false false
                           with
                           | Some res -> render_score_result res
                           | None ->
                               "<div style='color:#ff7675; font-size:1.5em; \
                                margin:20px;'>⚠️ No Yaku (诈和)</div>"
                         in

                         Dream.html
                           (Printf.sprintf
                              "<html><head><meta \
                               charset='utf-8'><title>Ron!</title><style>body \
                               { background-color: #2d3436; color: white; \
                               font-family: 'Segoe UI', sans-serif; \
                               text-align: center; padding-top: 50px; } .card \
                               { background: #333; display: inline-block; \
                               padding: 40px; border-radius: 15px; box-shadow: \
                               0 10px 30px rgba(0,0,0,0.5); max-width: 600px; \
                               } h1 { color: #fdcb6e; font-size: 4em; \
                               margin-bottom: 10px; text-shadow: 0 0 20px \
                               #e17055; } .loser { color: #ff7675; \
                               font-weight: bold; font-size: 1.2em; margin: \
                               20px 0; } a { color: #74b9ff; text-decoration: \
                               none; font-size: 1.2em; border: 1px solid \
                               #74b9ff; padding: 10px 30px; border-radius: \
                               30px; transition: 0.3s; } a:hover { background: \
                               #74b9ff; color: #2d3436; } \
                               </style></head><body><div class='card'><h1>⚡ \
                               Ron! ⚡</h1><h2>🎉 Winner: %s</h2><div \
                               class='loser'>💥 Loser: %s</div><div \
                               style='margin: 30px 0; font-size: 1.5em;'>%s \
                               <span style='border: 2px solid #fdcb6e; \
                               padding: 2px 8px; border-radius: 4px; \
                               margin-left: 10px; background:#fdcb6e; \
                               color:#2d3436;'>%s</span></div> %s <p \
                               style='margin-top: 50px;'><a \
                               href='/new_game'>Play \
                               Again</a></p></div></body></html>"
                              (Player.name winner) (Player.name loser)
                              (Hand.to_string (Player.hand winner))
                              (Tile.to_string tile) score_html)
                     | None -> Dream.redirect request "/")
                 | None -> Dream.redirect request "/")
             | _ -> Dream.redirect request "/");
         (* POST /win : Handles the Tsumo (Self-draw) win condition. *)
         Dream.post "/win" (fun _ ->
             let game = !game_state_ref in
             let all_p = Game.all_players game in
             let p0 = List.nth all_p 0 in
             let indicators = Game.get_dora_indicators game in

             match
               Hand.calculate_score (Player.hand p0) (Player.melds p0)
                 indicators Tile.East Tile.East true false
             with
             | Some res ->
                 let score_html = render_score_result res in
                 Dream.html
                   (Printf.sprintf
                      "<html><head><meta \
                       charset='utf-8'><title>Tsumo!</title><style>body { \
                       background-color: #2d3436; color: white; font-family: \
                       'Segoe UI', sans-serif; text-align: center; \
                       padding-top: 50px; } .card { background: #333; display: \
                       inline-block; padding: 40px; border-radius: 15px; \
                       box-shadow: 0 10px 30px rgba(0,0,0,0.5); max-width: \
                       600px; } h1 { color: #d63031; font-size: 4em; \
                       margin-bottom: 10px; text-shadow: 0 0 20px #e17055; } a \
                       { color: #74b9ff; text-decoration: none; font-size: \
                       1.2em; border: 1px solid #74b9ff; padding: 10px 30px; \
                       border-radius: 30px; transition: 0.3s; } a:hover { \
                       background: #74b9ff; color: #2d3436; } \
                       </style></head><body><div class='card'><h1>🎉 Tsumo! \
                       🎉</h1><h2>Winner: %s</h2><div style='font-size: 2em; \
                       margin: 30px 0;'>%s</div>%s<p style='margin-top: \
                       50px;'><a href='/new_game'>Play \
                       Again</a></p></div></body></html>"
                      (Player.name p0)
                      (Hand.to_string (Player.hand p0))
                      score_html)
             | None ->
                 Dream.html
                   "<html><body style='background:#2d3436; color:white; \
                    text-align:center; padding-top:50px;'><h1>⚠️ Cannot Win (No \
                    Yaku / 没役)</h1><p>Check your hand again.</p><a href='/' \
                    style='color:#74b9ff'>Back</a></body></html>");
         (* Resets the game state. *)
         Dream.get "/new_game" (fun req ->
             game_state_ref := Game.create ();
             Dream.redirect req "/");
       ]
