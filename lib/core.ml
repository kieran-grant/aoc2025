let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

(* Euclidean algorithm for gcd of two ints *)
let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)

(* gcd of a list of ints *)
let gcd_list lst =
  match lst with
  | [] -> invalid_arg "gcd_list: empty list"
  | x :: xs -> List.fold_left gcd x xs

let all_equal = function
  | [] | [ _ ] -> true
  | x :: xs -> List.for_all (( = ) x) xs

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

let range a b = List.init (b - a) (( + ) a)
let rec sum l = match l with [] -> 0 | hd :: tl -> hd + sum tl

(**[explode s] takes in a string and returns it as a list of char*)
let explode s = List.init (String.length s) (String.get s)

(**[implode l] takes in a list of char and returns it as a string*)
let implode l = String.of_seq (List.to_seq l)

let max_of_list lst = List.fold_left max 0 lst
let digit_of_char c = Char.code c - Char.code '0'

let char_of_digit n =
  if n >= 0 && n <= 9 then Char.chr (n + Char.code '0')
  else invalid_arg "digit_of_int: not a single digit"
