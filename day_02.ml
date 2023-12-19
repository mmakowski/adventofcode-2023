#use "str";;

type cube_set = {
  red : int;
  green : int;
  blue : int
};;

type game = {
  id : int;
  draws : cube_set list
};;

let pp_cube_set out cs =
  Format.fprintf out "r: %d g: %d b: %d" cs.red cs.green cs.blue
;;

let pp_game out game =
  Format.printf "%d: %a" game.id Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out "; ") pp_cube_set) game.draws
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

let min_cube_set game =
  let update acc draw =
    { 
      red = max acc.red draw.red;
      green = max acc.green draw.green;
      blue = max acc.blue draw.blue
    }
  in
    List.fold_left update {red = 0; green = 0; blue = 0} game.draws
;;

let cube_set_power cs =
  cs.red * cs.green * cs.blue
;;

let line_value line =
  let game = parse_game line
  in 
    (* debug_game game; *)
    (* Part One: *)
    (* if game_possible game then game.id else 0 *)
    (* Part Two: *)
    cube_set_power (min_cube_set game)
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
