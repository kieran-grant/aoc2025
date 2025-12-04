open Core

let get_max_and_idx lst =
  let max_elem = max_of_list lst in
  let idx_of_max =
    match List.find_index (( = ) max_elem) lst with
    | Some n -> n
    | _ -> failwith "No element found"
  in
  (max_elem, idx_of_max)

let rec inner_loop target curr_list window_size acc =
  match curr_list with
  | [] -> acc
  | _ when List.length acc = target -> acc
  (*TODO: We need another guard here, something like:


  *)
  | _ ->
      let sub_list = List.take window_size curr_list in
      let v, idx = get_max_and_idx sub_list in
      let new_list = List.drop (idx + 1) curr_list in
      let new_acc = List.concat [ acc; [ v ] ] in
      inner_loop target new_list window_size new_acc

let solve_for n list = inner_loop n list (List.length list - (n - 1)) []

let solver_pipeline (n : int) (input : string) =
  (*Turn string into list of chars*)
  Core.explode input
  (*Turn list of chars into list of ints*)
  |> List.map digit_of_char
  (*Run solver on list*)
  |> solve_for n
  (*Convert ints back to chars*)
  |> List.map char_of_digit
  (*Convert list of chars to string*)
  |> implode
  (*Convert string to int*)
  |> int_of_string

let solve_for_all (n : int) (inputs : string list) =
  List.map (solver_pipeline n) inputs |> sum

(*Part 1, pick batteries*)
let part_1 = solve_for_all 2

(*Part 2, pick 12 batteries*)
let part_2 = solve_for_all 12
