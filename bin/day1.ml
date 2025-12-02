let sample_1 = {|L68
L30
R48
L5
R60
L55
L1
L99
R14
L82|}

type instruction = { direction : char; amount : int }

let positive_mod a n = ((a mod n) + n) mod n

(**[explode s] takes in a string and returns it as a list of char*)
let explode s = List.init (String.length s) (String.get s)

(**[implode l] takes in a list of char and returns it as a string*)
let implode l = String.of_seq (List.to_seq l)

(**[head_and_tail_of_string] takes a string and returns the first character and
   the rest of the string*)
let head_and_tail_of_string input =
  match explode input with
  (*TODO: probably need a better alternative for first two arms*)
  | [] -> (' ', "")
  | x :: [] -> (x, "")
  | x :: xs -> (x, implode xs)

let parse_line input =
  let dir, amount = head_and_tail_of_string input in
  { direction = dir; amount = int_of_string amount }

let get_next_state curr_state instruction =
  let op =
    match instruction.direction with
    | 'L' -> ( - )
    | 'R' -> ( + )
    | _ -> failwith "Unexpected dir"
  in
  op curr_state instruction.amount

let rec part_1' curr_state instructions acc =
  match instructions with
  | [] -> acc
  | x :: xs ->
      let new_state = positive_mod (get_next_state curr_state x) 100 in
      if new_state = 0 then part_1' new_state xs (acc + 1)
      else part_1' new_state xs acc

let part_1 input_string = part_1' 50 (List.map parse_line input_string) 0

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let sample_input = String.split_on_char '\n' sample_1
let input = read_lines "bin/input.txt"
let sol_1 = part_1 input
let sample_sol_1 = part_1 sample_input

let () =
  print_endline ("Part 1 (sample): " ^ string_of_int sample_sol_1);
  print_endline ("Part 1: " ^ string_of_int sol_1)
