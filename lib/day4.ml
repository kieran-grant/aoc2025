open Core

let directions =
  [
    (* East       *)
    (0, 1);
    (* South-East *)
    (1, 1);
    (* South      *)
    (1, 0);
    (* South-West *)
    (1, -1);
    (* West       *)
    (0, -1);
    (* North-West *)
    (-1, -1);
    (* North      *)
    (-1, 0);
    (* North-East *)
    (-1, 1);
  ]

let is_a_roll map idx = get_opt_entry map idx = Some '@'
let get_roll_value map idx = if is_a_roll map idx then 1 else 0

let get_score map index =
  List.map (fun dir -> get_roll_value map (add_idx index dir)) directions |> sum

let is_accessible map idx = get_score map idx < 4

let part_1 entries =
  let map = create_map entries in
  (*Get all the indices in the grid map*)
  keys map
  (*Filter out things that aren't a roll*)
  |> List.filter (is_a_roll map)
  (*Filter out rolls that are inaccessible*)
  |> List.filter (is_accessible map)
  (*Count the number of accessible rolls*)
  |> List.length
