open Core

let parse_input raw_txt =
  let lines = String.split_on_char '\n' raw_txt in
  let rows =
    lines |> List.map split_on_whitespace |> List.filter (fun row -> row <> [])
  in
  rows |> transpose |> List.map List.rev

let get_accumulator_fn x =
  match x with
  | "+" -> List.fold_left ( + ) 0
  | "*" -> List.fold_left ( * ) 1
  | _ -> failwith ("Unknown fn: " ^ x)

let get_answer curr_list =
  match curr_list with
  | x :: xs -> (get_accumulator_fn x) (List.map int_of_string xs)
  | _ -> 0

let part_1 lines = List.map get_answer lines |> sum
