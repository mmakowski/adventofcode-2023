(**
#use "topfind";;
#require "str";;
#use "day_01.ml";;
*)

let digit search_fn str =
  let digit_regexp = Str.regexp {|[0-9]\|zero\|one\|two\|three\|four\|five\|six\|seven\|eight\|nine|} in
  search_fn digit_regexp str;
  match Str.matched_string str with
    | "zero" -> "0"
    | "one" -> "1"
    | "two" -> "2"
    | "three" -> "3"
    | "four" -> "4"
    | "five" -> "5"
    | "six" -> "6"
    | "seven" -> "7"
    | "eight" -> "8"
    | "nine" -> "9"
    | x when String.length x = 1 -> x
    | other -> raise (Invalid_argument other)

let line_value line =
  let first_digit = digit (fun regexp str -> Str.search_forward regexp str 0) line in
  let last_digit = digit (fun regexp str -> Str.search_backward regexp str (String.length str)) line in
  let num_str = String.cat first_digit last_digit in
  let num = int_of_string num_str in
  (* Printf.printf "%d\n" num; *)
  num
;;

let rec process_lines f acc =
  try 
    let line = input_line f
    in process_lines f (acc + line_value line)
  with End_of_file ->
    close_in_noerr f;
    acc
;;

let result () = 
  let f = open_in "input-01.txt"
  in process_lines f 0
;;

Printf.printf "%d\n" (result ());;
