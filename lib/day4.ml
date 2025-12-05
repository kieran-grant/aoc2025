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

let get_accessible_indices map =
  (*Get all the indices in the grid map*)
  keys map
  (*Filter out things that aren't a roll*)
  |> List.filter (is_a_roll map)
  (*Filter out rolls that are inaccessible*)
  |> List.filter (is_accessible map)

let part_1 entries =
  let map = create_map entries in
  (*Count the number of accessible rolls*)
  get_accessible_indices map |> List.length

let remove_rolls map indices = replace_many map indices '.'

let rec part_2_loop curr_map acc =
  match get_accessible_indices curr_map with
  (*No more accessible rolls, return the accumulator*)
  | [] -> acc
  (*Remove the rolls that are accessible, add their count to the accumulator*)
  | xs -> part_2_loop (remove_rolls curr_map xs) (acc + List.length xs)

let part_2 entries =
  let map = create_map entries in
  part_2_loop map 0
