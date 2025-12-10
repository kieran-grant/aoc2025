open Core

let get_max_and_idx lst =
  let max_elem = max_of_list lst in
  let idx_of_max =
    match List.find_index (( = ) max_elem) lst with
    | Some n -> n
    | _ -> failwith "No element found"
  in
  (max_elem, idx_of_max)

let calculate_window_size list target acc =
  let list_len = List.length list in
  let acc_len = List.length acc in
  let answer = list_len - (target - (acc_len + 1)) in
  answer

let rec inner_loop target curr_list acc =
  let window_size = calculate_window_size curr_list target acc in
  match curr_list with
  | [] -> acc
  (*If we have the target number of elements in the accumulator, return*)
  | _ when List.length acc = target -> acc
  (*If the window size is 1, we can just short circuit as there is no real logic to perform*)
  | _ when window_size = 1 -> acc @ curr_list
  | _ ->
      (*Take first [window_size] elements of the list*)
      let sub_list = List.take window_size curr_list in
      (*Find the max value of this and its index*)
      let v, idx = get_max_and_idx sub_list in
      (*Trim the first [idx + 1] elements from the list to create next list*)
      let new_list = List.drop (idx + 1) curr_list in
      (*Add the max value we found to the accumulator*)
      let new_acc = acc @ [ v ] in
      (*Recurse*)
      inner_loop target new_list new_acc

let solve_for n list = inner_loop n list []

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
