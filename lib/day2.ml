let split_string string =
  let len = String.length string in
  let mid = len / 2 in
  let first = String.sub string 0 mid in
  let second = String.sub string mid (len - mid) in
  (first, second)

let is_invalid num_string =
  String.length num_string mod 2 = 0
  &&
  let f, s = split_string num_string in
  f = s

(*TODO: 
For part 2, we want to check whether the number is completely made up of subsequences repeated at least twice
- To filter early, we can notice that the counts of all occurances of each number must be even
- Can we use these occrances to get a hint about what number of subsequences we have? Is it the min of the number of occurances?
e.g. using the examples from the spec:
So, 12341234 (1234 two times), 123123123 (123 three times), 1212121212 (12 five times), and 1111111 (1 seven times) are all invalid IDs.

12341234 
- 1, 2, 3, 4 all appear twice -> it is made up of two subsequences
- 1, 2, 3 all appear three times -> it is made  up of three subsequences
- 1, 2 all appear 5 times -> it is made up of 5 subsequences
- 1 appeaers 7 times -> it is made up of 7 subsequences

Okay, what about one where the number of occurances is slightly differenct, e.g
- 111211121112 -> 1 appears 9 times, 2 appears 3 times -> it is made up of three sub sequences


okay, so our invalid check is now:
- Has to be even
- The number of occurances of each digit must be even
- Split the string up into [min number of ocurrances] chunks, is invalid if all chunks are the same
*)

let count_digits s =
  let counts = Array.make 10 0 in
  String.iter
    (fun c ->
      if '0' <= c && c <= '9' then
        counts.(Char.code c - Char.code '0') <-
          counts.(Char.code c - Char.code '0') + 1)
    s;
  List.filter (fun x -> x > 0) (Array.to_list counts)

let rec min_elem lst =
  match lst with
  | [] -> failwith "Empty list"
  | [ x ] -> x
  | x :: xs -> min x (min_elem xs)

(* Euclidean algorithm for gcd of two ints *)
let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)

(* gcd of a list of ints *)
let gcd_list lst =
  match lst with
  | [] -> invalid_arg "gcd_list: empty list"
  | x :: xs -> List.fold_left gcd x xs

let chunk_string s k =
  let n = String.length s in
  let chunk_size = n / k in
  List.init k (fun i -> String.sub s (i * chunk_size) chunk_size)

let all_equal = function
  | [] | [ _ ] -> true
  | x :: xs -> List.for_all (( = ) x) xs

let are_all_equal_subsequences num_string len =
  let chunks = chunk_string num_string len in
  all_equal chunks

let divisors n =
  if n <= 0 then invalid_arg "divisors: n must be positive"
  else
    let rec aux i acc =
      if i * i > n then acc
      else if n mod i = 0 then
        let j = n / i in
        if i = j then aux (i + 1) (i :: acc) (* perfect square *)
        else aux (i + 1) (i :: j :: acc)
      else aux (i + 1) acc
    in
    List.rev (aux 1 [])

let try_divisors num_string greatest_common_divisor =
  let divisors_of_gcd =
    List.filter (( < ) 1) (divisors greatest_common_divisor)
  in
  let rec aux dvs =
    match dvs with
    | [] -> false (* return false if no divisor worked *)
    | x :: xs ->
        if are_all_equal_subsequences num_string x then true else aux xs
  in
  aux divisors_of_gcd

let is_invalid_v2 num_string =
  let occurances = count_digits num_string in
  if List.length occurances = 1 then List.nth occurances 0 >= 2
  else
    let common_divisor = gcd_list occurances in
    if common_divisor = 1 then false else try_divisors num_string common_divisor

let is_valid num_string = not (is_invalid num_string)
let range a b = List.init (b - a) (( + ) a)

let parse_range input =
  let items = String.split_on_char '-' input in
  let start = int_of_string (List.nth items 0) in
  let stop = int_of_string (List.nth items 1) in
  range start (stop + 1)

let parse_input input = String.split_on_char ',' input
let get_invalid_strings strings = List.filter is_invalid strings
let get_invalid_strings_v2 strings = List.filter is_invalid_v2 strings
let rec sum l = match l with [] -> 0 | hd :: tl -> hd + sum tl

let part_1 input =
  (*Split string by comma*)
  parse_input input
  (*Change n-m to a list [n, n+1, ..., m-1, m] *)
  |> List.map parse_range
  (*Flatten list of lists into a list of ints*)
  |> List.flatten
  (*Convert ints to strings*)
  |> List.map string_of_int
  (*Get the invalid items*)
  |> get_invalid_strings
  (*Convert back to int*)
  |> List.map int_of_string
  (*Get the total*)
  |> sum

let part_2 input =
  (*Split string by comma*)
  parse_input input
  (*Change n-m to a list [n, n+1, ..., m-1, m] *)
  |> List.map parse_range
  (*Flatten list of lists into a list of ints*)
  |> List.flatten
  (*Convert ints to strings*)
  |> List.map string_of_int
  (*Get the invalid items*)
  |> get_invalid_strings_v2
  (*Convert back to int*)
  |> List.map int_of_string
  (*Get the total*)
  |> sum
