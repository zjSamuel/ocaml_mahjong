

type t = Tile.t list

let empty = []
let sort hand = List.sort Tile.compare hand
let add hand tile = sort (tile :: hand)

let to_string hand =
  if hand = [] then "(empty)"
  else hand |> List.map Tile.to_string |> String.concat " "

let remove_first hand tile =
  let rec aux acc = function
    | [] -> None
    | h :: t ->
        if Tile.compare h tile = 0 then Some (List.rev acc @ t)
        else aux (h :: acc) t
  in
  aux [] hand

let max_melds_ref = ref 4

let tile_to_id = function
  | Tile.Numbered (Tile.Man, n) -> n - 1
  | Tile.Numbered (Tile.Pin, n) -> 9 + (n - 1)
  | Tile.Numbered (Tile.Sou, n) -> 18 + (n - 1)
  | Tile.Honor h ->
      27
      + (match h with
        | Tile.East -> 0
        | Tile.South -> 1
        | Tile.West -> 2
        | Tile.North -> 3
        | Tile.Red -> 4
        | Tile.Green -> 5
        | Tile.White -> 6)

let all_tile_types =
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
  numbered @ List.map (fun h -> Tile.Honor h) honors

let to_frequency_table hand =
  let counts = Array.make 34 0 in
  List.iter
    (fun t ->
      let id = tile_to_id t in
      counts.(id) <- counts.(id) + 1)
    hand;
  counts

let count_tatsu_greedy counts =
  let c = Array.copy counts in
  let tatsu = ref 0 in
  for i = 0 to 26 do
    while i mod 9 < 8 && c.(i) > 0 && c.(i + 1) > 0 do
      incr tatsu;
      c.(i) <- c.(i) - 1;
      c.(i + 1) <- c.(i + 1) - 1
    done;
    while i mod 9 < 7 && c.(i) > 0 && c.(i + 2) > 0 do
      incr tatsu;
      c.(i) <- c.(i) - 1;
      c.(i + 2) <- c.(i + 2) - 1
    done
  done;
  for i = 0 to 33 do
    while c.(i) >= 2 do
      incr tatsu;
      c.(i) <- c.(i) - 2
    done
  done;
  !tatsu

let rec search_max_score counts depth current_m =
  if depth >= 34 then
    let t = count_tatsu_greedy counts in
    let effective_tatsu = min t (!max_melds_ref - current_m) in
    (current_m * 2) + effective_tatsu
  else if counts.(depth) = 0 then search_max_score counts (depth + 1) current_m
  else
    let best = ref (-1) in
    if counts.(depth) >= 3 then (
      counts.(depth) <- counts.(depth) - 3;
      best := max !best (search_max_score counts depth (current_m + 1));
      counts.(depth) <- counts.(depth) + 3);
    if depth < 27 && depth mod 9 < 7 then
      if counts.(depth + 1) > 0 && counts.(depth + 2) > 0 then (
        counts.(depth) <- counts.(depth) - 1;
        counts.(depth + 1) <- counts.(depth + 1) - 1;
        counts.(depth + 2) <- counts.(depth + 2) - 1;
        best := max !best (search_max_score counts depth (current_m + 1));
        counts.(depth) <- counts.(depth) + 1;
        counts.(depth + 1) <- counts.(depth + 1) + 1;
        counts.(depth + 2) <- counts.(depth + 2) + 1);
    best := max !best (search_max_score counts (depth + 1) current_m);
    !best

let calculate_shanten hand =
  let hand_len = List.length hand in
  max_melds_ref := hand_len / 3;
  let base = !max_melds_ref * 2 in
  let counts = to_frequency_table hand in
  let min_shanten = ref 8 in
  for i = 0 to 33 do
    if counts.(i) >= 2 then (
      counts.(i) <- counts.(i) - 2;
      let score = search_max_score counts 0 0 in
      let shanten = base - score - 1 in
      min_shanten := min !min_shanten shanten;
      counts.(i) <- counts.(i) + 2)
  done;
  let score_no_pair = search_max_score counts 0 0 in
  min_shanten := min !min_shanten (base - score_no_pair);
  !min_shanten

let calc_ukeire_count hand_13 =
  let current_shanten = calculate_shanten hand_13 in
  let effective_count = ref 0 in
  List.iter
    (fun tile ->
      let temp_hand = tile :: hand_13 in
      let new_shanten = calculate_shanten temp_hand in
      if new_shanten < current_shanten then
        let count_in_hand =
          List.filter (fun t -> Tile.compare t tile = 0) hand_13 |> List.length
        in
        let possible = 4 - count_in_hand in
        if possible > 0 then effective_count := !effective_count + possible)
    all_tile_types;
  !effective_count

let calculate_efficiency hand_14 =
  let base_shanten = calculate_shanten hand_14 in
  let unique_tiles = List.sort_uniq Tile.compare hand_14 in
  let results =
    List.filter_map
      (fun tile ->
        match remove_first hand_14 tile with
        | None -> None
        | Some hand_13 ->
            let sh13 = calculate_shanten hand_13 in
            if sh13 > base_shanten then
              None
            else
              let count = calc_ukeire_count hand_13 in
              Some (tile, count))
      unique_tiles
  in
  List.sort (fun (_, a) (_, b) -> compare b a) results

let is_complete hand = calculate_shanten hand <= -1
let possible_sets _ = []
