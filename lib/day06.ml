open Core

let get_accumulator_fn x =
  match x with
  | "+" -> List.fold_left ( + ) 0
  | "*" -> List.fold_left ( * ) 1
  | _ -> failwith ("Unknown fn: " ^ x)

let get_answer curr_list =
  match curr_list with
  | x :: xs -> (get_accumulator_fn x) (List.map int_of_string xs)
  | _ -> 0

(*
Turns a list of strings with empty elements to a 
list of list of strings split on those empty elements
*)
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

(*Kind of like a zip, takes prepends the elements of [ys] individually to lists of [xs]*)
let prepend_each (ys : string list) (xs : string list list) =
  let rec aux xs ys acc =
    match (xs, ys) with
    | x :: xs, y :: ys -> aux xs ys ((y :: x) :: acc)
    | [], [] -> List.rev acc
    | _ -> failwith "mismatched lists"
  in
  aux xs ys []

let part_1_parser lines =
  (*Then split on any whitespace in each line*)
  let rows = List.map split_on_whitespace lines in
  rows
  (*Turn columns to rows*)
  |> transpose
  (*Flip horizontally, makes pattern matching easier later*)
  |> List.map List.rev

let part_2_parser lines =
  (*
  Pull out the operators to add back in later, 
  makes transpose and splitting easier
  *)
  let operators, rest = last_and_rest lines in
  rest
  (*Turn list of string to list of list of chars*)
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
  |> prepend_each (split_on_whitespace operators)

let part_maker parser_fn raw_input =
  raw_input |> split_lines
  (*Parse input based on part*)
  |> parser_fn
  (*Do the sums*)
  |> List.map get_answer
  (*Use sum to accumulate the answer*)
  |> sum

let part_1 = part_maker part_1_parser
let part_2 = part_maker part_2_parser
