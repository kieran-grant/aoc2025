open Core

let parse_input raw_txt =
  let lines =
    (*Split on lines*)
    String.split_on_char '\n' raw_txt
    (*Get rid of any empty lines*)
    |> List.filter (fun x -> x <> "")
    (*Then split on any whitespace in each line*)
    |> List.map split_on_whitespace
  in
  lines
  (*Turn columns to rows*)
  |> transpose
  (*Flip horizontally, makes pattern matching easier later*)
  |> List.map List.rev

let get_accumulator_fn x =
  match x with
  | "+" -> List.fold_left ( + ) 0
  | "*" -> List.fold_left ( * ) 1
  | _ -> failwith ("Unknown fn: " ^ x)

let get_answer curr_list =
  match curr_list with
  | x :: xs -> (get_accumulator_fn x) (List.map int_of_string xs)
  | _ -> 0

let rec unsnoc = function
  | [] -> failwith "Empty list!"
  | [ x ] -> (x, [])
  | x :: xs ->
      let last, rest = unsnoc xs in
      (last, x :: rest)

let split_on_empty string_list =
  let rec aux curr_list curr_acc acc =
    match curr_list with
    | [] ->
        (* End of input: push the current group if non-empty *)
        List.rev curr_acc :: acc
    | x :: xs when x = "" ->
        (* Empty string: finish current group and start a new one *)
        aux xs [] (List.rev curr_acc :: acc)
    | y :: ys ->
        (* Normal element: accumulate it *)
        aux ys (y :: curr_acc) acc
  in
  (* Compute and then reverse the final result so groups are in correct order *)
  List.rev (aux string_list [] [])

let prepend_each (ys : string list) (xs : string list list) =
  let rec aux xs ys acc =
    match (xs, ys) with
    | x :: xs, y :: ys -> aux xs ys ((y :: x) :: acc)
    | [], [] -> List.rev acc
    | _ -> failwith "mismatched lists"
  in
  aux xs ys []

let new_parse raw_txt =
  let lines =
    String.split_on_char '\n' raw_txt |> List.filter (fun x -> x <> "")
  in
  let last_row, rest = unsnoc lines in
  rest
  (*Turn string[] to char[][]*)
  |> List.map explode
  (*Transpose it*)
  |> transpose
  (*Turn it back into string[] (with some trailing whitespace)*)
  |> List.map implode
  (*Trim the whitespace*)
  |> List.map String.trim
  (*Turn string[] to string[][] by splitting on empty string*)
  |> split_on_empty
  (*Prepend each of the operators*)
  |> prepend_each (split_on_whitespace last_row)

let part_maker parser_fn raw_input =
  raw_input
  (*Parse input based on part*)
  |> parser_fn
  (*Do the sums*)
  |> List.map get_answer
  (*Use sum to accumulate the answer*)
  |> sum

let part_1 = part_maker parse_input
let part_2 = part_maker new_parse
