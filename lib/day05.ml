open Core

(*Convert list of strings of form "a-b" to list of int tuples (a,b)*)
let get_ranges ranges = List.map parse_hypenated_ints ranges

let is_in_a_range (ranges : (int * int) list) item =
  List.exists (fun (l, u) -> l <= item && item <= u) ranges

(* Not sure why, but some blank strings were getting through here *)
let convert_to_ints ids =
  ids |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string

let part_1 (ranges : string list) (ids : string list) =
  let fresh_ranges = get_ranges ranges in
  convert_to_ints ids |> List.filter (is_in_a_range fresh_ranges) |> List.length

(* Merge two overlapping or touching intervals *)
let merge (a1, b1) (a2, b2) = if b1 >= a2 then Some (a1, max b1 b2) else None

let merge_intervals intervals =
  (* Recursively merge intervals *)
  let rec aux intervals acc =
    match intervals with
    | [] -> List.rev acc
    | [ x ] -> List.rev (x :: acc)
    | x :: y :: xs -> (
        match merge x y with
        | Some merged -> aux (merged :: xs) acc
        | None -> aux (y :: xs) (x :: acc))
  in
  let sorted = List.sort compare intervals in
  aux sorted []

let part_2 (ranges : string list) =
  let fresh_ranges = get_ranges ranges in
  (*Merge ranges that overlap*)
  let adjusted_ranges = merge_intervals fresh_ranges in
  (*Number of ints in range is just upper - lower + 1*)
  List.map (fun (a, b) -> b - a + 1) adjusted_ranges |> sum
