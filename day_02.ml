(**
#use "topfind";;
#require "str";;
#use "day_02.ml";;
*)

type draw = {
  red : int;
  green : int;
  blue : int
};;

type game = {
  id : int;
  draws : draw list
};;

let pp_draw out draw =
  Format.fprintf out "r: %d g: %d b: %d" draw.red draw.green draw.blue
;;

let pp_game out game =
  Format.printf "%d: %a" game.id Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "; ") pp_draw) game.draws
;;

let debug_game game =
  Format.printf "%a\n" pp_game game

(* 1 red, 2 green, 6 blue *)
let parse_draw str =
  let num colour =
    let regexp = Str.regexp ({|\([0-9]+\) |} ^ colour) in
    try 
      let _ = Str.search_forward regexp str 0 
      in int_of_string (Str.matched_group 1 str)
    with Not_found -> 0
  in {red = num "red"; green = num "green"; blue = num "blue"}
;;

(* 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green *)
let parse_draws str =
  List.map parse_draw (String.split_on_char ';' str)
;;

(* Game 123 *)
let parse_id str =
  let regexp = Str.regexp {|Game \([0-9]+\)|} in
  if not (Str.string_match regexp str 0) then 
    invalid_arg str
  else
    int_of_string (Str.matched_group 1 str)
;;

(* Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green *)
let parse_game str =
  match String.split_on_char ':' str with
    | id_str :: draws_str :: [] -> {id = parse_id id_str; draws = parse_draws draws_str}
    | other -> invalid_arg (String.concat ", " other)
;;

let draw_possible draw = 
  draw.red <= 12 && draw.green <= 13 && draw.blue <= 14
;;

let game_possible game =
  List.for_all draw_possible game.draws
;;

let line_value line =
  let game = parse_game line
  in 
    (* debug_game game; *)
    if game_possible game then game.id else 0
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
  let f = open_in "input-02.txt"
  in process_lines f 0
;;

Printf.printf "%d\n" (result ());;
