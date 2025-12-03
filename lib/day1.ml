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

let parse_rotation { direction; amount } =
  match direction with
  | 'L' -> -amount
  | 'R' -> amount
  | _ -> failwith "Unrecognised rotation"

let rec part_1' curr_state instructions acc =
  match instructions with
  | [] -> acc
  | x :: xs ->
      let new_state = positive_mod (parse_rotation x + curr_state) 100 in
      let new_acc = if new_state = 0 then acc + 1 else acc in
      part_1' new_state xs new_acc

let part_1 instructions = part_1' 50 instructions 0

let calculate_clicks curr_state raw_rotation =
  (*parse the rotation into either a positive or negative number*)
  let rotation = parse_rotation raw_rotation in
  (*add old state to new state to get raw number*)
  let new_state_before = rotation + curr_state in
  (*new state is raw state mod 100*)
  let new_state = positive_mod new_state_before 100 in
  (*the total number of clicks should be the distace between the old state and new state divided by 100*)
  let total_full_loops = abs (new_state_before - curr_state) / 100 in
  let wrapped =
    (raw_rotation.direction = 'L' && new_state > curr_state)
    || (raw_rotation.direction = 'R' && new_state < curr_state)
  in
  let extra_crossings =
    (*If we end on a zero, count it*)
    if new_state = 0 then 1
      (*If we were already on a zero, then won't count as a crossing*)
    else if curr_state = 0 then 0
      (*If we have to go through zero to meet the new state, then we cross*)
    else if wrapped then 1
    else 0
  in
  (new_state, total_full_loops + extra_crossings)

let rec part_2' curr_state instructions acc =
  match instructions with
  | [] -> acc
  | instruction :: instructions ->
      let new_state, num_clicks = calculate_clicks curr_state instruction in
      part_2' new_state instructions acc + num_clicks

let part_2 instructions = part_2' 50 instructions 0
let parse_input raw_text = List.map parse_line raw_text
