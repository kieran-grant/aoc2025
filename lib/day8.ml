open Core

type point3d = float * float * float

let distance ((a, b, c) : point3d) ((x, y, z) : point3d) =
  sqrt (((a -. x) ** 2.) +. ((b -. y) ** 2.) +. ((c -. z) ** 2.))

let to_point3d lst =
  match lst with
  | [ x; y; z ] -> (x, y, z)
  | _ -> failwith "The given list cannot be converted to a 3d point"

let rec pairs = function
  | [] -> []
  | x :: xs ->
      (* all pairs where x is paired with each element of xs *)
      let with_x = List.map (fun y -> (x, y)) xs in
      (* plus all pairs that come from the tail *)
      with_x @ pairs xs

let parse_input raw_input =
  split_lines raw_input
  |> List.map (String.split_on_char ',')
  |> List.map (List.map float_of_string)
  |> List.map to_point3d

let sort_by_distance pairs =
  let distances = List.map (fun (x, y) -> ((x, y), distance x y)) pairs in
  let sorted = List.sort (fun (_, d1) (_, d2) -> compare d1 d2) distances in
  List.map (fun (a, _) -> a) sorted

let get_pairs_by_distance (lst : point3d list) =
  let all_pairs = pairs lst in
  sort_by_distance all_pairs

module Point3DOrd = struct
  type t = point3d

  let compare (x1, y1, z1) (x2, y2, z2) =
    match Float.compare x1 x2 with
    | 0 -> ( match Float.compare y1 y2 with 0 -> Float.compare z1 z2 | c -> c)
    | c -> c
end

module Point3DSet = Set.Make (Point3DOrd)

(*Get index of the set which contains the item, or None if it isn't in any*)
let get_member_idx (sets : Point3DSet.t list) (item : point3d) =
  let rec aux sets item idx =
    match sets with
    | [] -> None
    | s :: ss ->
        if Point3DSet.mem item s then Some idx else aux ss item (idx + 1)
  in
  aux sets item 0

(* Add point to set at index *)
let add_to_set_at_idx circuits idx point =
  List.mapi (fun i s -> if i = idx then Point3DSet.add point s else s) circuits

(* Merge sets at indices i and j and return the updated list *)
let merge_sets_at_indices circuits i j =
  if i = j then circuits
  else
    let set_i = List.nth circuits i in
    let set_j = List.nth circuits j in
    let merged_set = Point3DSet.union set_i set_j in
    circuits
    |> List.mapi (fun k s -> if k = i then merged_set else s)
    |> List.mapi (fun k s -> if k = j then Point3DSet.empty else s)
    |> List.filter (fun s -> not (Point3DSet.is_empty s))

let get_circuits (pairs : (point3d * point3d) list) =
  let rec aux remaining_pairs (circuits : Point3DSet.t list) =
    match remaining_pairs with
    | [] -> circuits
    | (a, b) :: ps -> (
        let member_checker = get_member_idx circuits in
        match (member_checker a, member_checker b) with
        (*Points aren't part of any set, create a new one with those points in it*)
        | None, None -> aux ps (Point3DSet.of_list [ a; b ] :: circuits)
        (*Points are contained in two separate sets, should merge them*)
        | Some i, Some j ->
            if i = j then aux ps circuits
            else aux ps (merge_sets_at_indices circuits i j)
        (*Point a in a set but point b isn't, add b to a's set*)
        | Some i, None -> aux ps (add_to_set_at_idx circuits i b)
        | None, Some j -> aux ps (add_to_set_at_idx circuits j a))
  in
  aux pairs []

let get_circuits_2 (pairs : (point3d * point3d) list) (n_points : int) =
  (*At each iteration, we check if:
  - We have a most recent pair (true in all but the first iteration)
  - There is only one set left and that set has all the points in it
  *)
  let answer circuits most_recent_pair =
    match most_recent_pair with
    | None -> None
    | Some pair ->
        if
          List.length circuits = 1
          && Point3DSet.cardinal (List.hd circuits) = n_points
        then Some pair
        else None
  in

  let rec aux remaining_pairs circuits most_recent_pair =
    (*If we have an answer return it*)
    match answer circuits most_recent_pair with
    | Some pair -> pair
    | None -> (
        match remaining_pairs with
        (*Have checked all pairs, but don't have all the points - something has gone wrong!*)
        | [] -> failwith "Ran out of pairs before full merge!"
        | (a, b) :: ps -> (
            (*Check whether the points exist in any sets*)
            let member_checker = get_member_idx circuits in
            match (member_checker a, member_checker b) with
            (*If they don't, create a set with just those points and continue*)
            | None, None ->
                let new_circuit = Point3DSet.of_list [ a; b ] in
                aux ps (new_circuit :: circuits) (Some (a, b))
            (*They are both in sets*)
            | Some i, Some j ->
                (*They are already in the same set (maybe via a previous merge), just continue*)
                if i = j then aux ps circuits most_recent_pair
                (*They are in different sets, merge them*)
                  else
                  let new_list = merge_sets_at_indices circuits i j in
                  aux ps new_list (Some (a, b))
            (*One is in a set, but the other isn't => add the singleton to the set*)
            | Some i, None ->
                let updated_circuits = add_to_set_at_idx circuits i b in
                aux ps updated_circuits (Some (a, b))
            | None, Some j ->
                let updated_circuits = add_to_set_at_idx circuits j a in
                aux ps updated_circuits (Some (a, b))))
  in
  aux pairs [] None

let part_2 raw_input =
  let parsed = parse_input raw_input in
  let sorted_pairs = get_pairs_by_distance parsed in
  let (x1, _, _), (x2, _, _) =
    get_circuits_2 sorted_pairs (List.length parsed)
  in
  int_of_float (x1 *. x2)

let part_1 raw_input n_shortest =
  let parsed = parse_input raw_input in
  let sorted = get_pairs_by_distance parsed in
  let circuits = get_circuits (List.take n_shortest sorted) in
  let cardinalities = List.map Point3DSet.cardinal circuits in
  let largest_sets =
    cardinalities |> List.sort compare |> List.rev |> List.take 3
  in
  List.fold_left ( * ) 1 largest_sets
