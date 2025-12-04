open Core

let max_of_list lst = List.fold_left max 0 lst

let get_max_and_idx lst =
  let max_elem = max_of_list lst in
  let idx_of_max =
    match List.find_index (( = ) max_elem) lst with
    | Some n -> n
    | _ -> failwith "No element found"
  in
  (max_elem, idx_of_max)

let solve_battery batteries =
  let initial_list = List.take (List.length batteries - 1) batteries in
  let max, idx_of_max = get_max_and_idx initial_list in
  let sublist = List.drop (idx_of_max + 1) batteries in
  let second_max = max_of_list sublist in
  (max * 10) + second_max

let part_1_pipeline line = explode line |> List.map int_of_char |> solve_battery
let part_1 input = List.map part_1_pipeline input |> sum
