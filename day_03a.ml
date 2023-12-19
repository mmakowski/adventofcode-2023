#require "str";;

type point = {
  x: int;
  y: int
}

type number = {
  value: int;
  start: point;
  width: int;
};;

module IntMap = Map.Make(Int);;

type schematic = {
  numbers_by_y: (number list) IntMap.t; (* y -> numbers in that line *)
  cogs: point list;
};;

let rec parse_numbers line y start_x acc =
  let num_regexp = Str.regexp {|[0-9]+|} in
  try
    let num_x = Str.search_forward num_regexp line start_x in
    let value_str = Str.matched_string line in
    let value_width = String.length value_str in
    let num = {
      value = int_of_string value_str; 
      start = {x = num_x; y = y}; 
      width = value_width
    } in
    parse_numbers line y (num_x+value_width) (num::acc)
  with Not_found -> List.rev acc
;;

let rec parse_cogs line y start_x acc =
  let part_regexp = Str.regexp {|\*|} in
  try
    let cog_x = Str.search_forward part_regexp line start_x in
    let cog = {x = cog_x; y = y} in
    parse_cogs line y (cog_x+1) (cog::acc)
  with Not_found -> List.rev acc
;;

let update_schematic schematic line y =
  let line_numbers = parse_numbers line y 0 [] in
  let line_cogs = parse_cogs line y 0 [] in
  {
    numbers_by_y = IntMap.add y line_numbers schematic.numbers_by_y;
    cogs = schematic.cogs @ line_cogs
  }
;;

let rec process_lines f schematic y =
  try 
    let line = input_line f
    in process_lines f (update_schematic schematic line y) (y + 1)
  with End_of_file ->
    close_in_noerr f;
    schematic
;;

let parse_schematic f =
  process_lines f {numbers_by_y = IntMap.empty; cogs = []} 0
;;

let adjacent_nums x = 
  let adjacent_value {value; start; width} =
    if x >= start.x - 1 && x <= start.x + width then
      Some value
    else
      None
  in function
    | None -> [] ;
    | Some nums -> List.filter_map adjacent_value nums
;;

let cog_nums nums_by_y cog_coords =
  let adj offset = adjacent_nums cog_coords.x (IntMap.find_opt (cog_coords.y + offset) nums_by_y)
  in (adj (-1)) @ (adj 0) @ (adj 1)
;;

let gear_ratio = function
  | a::b::[] -> a * b;
  | _ -> 0
;;

let gear_ratios schematic =
  let nums = List.map (cog_nums schematic.numbers_by_y) schematic.cogs in
  List.map gear_ratio nums
;;

let rec sum acc = function
  | [] -> acc
  | h::t -> sum (acc+h) t
;;

let result () = 
  let f = open_in "input-03.txt" in
  let schematic = parse_schematic f in
  sum 0 (gear_ratios schematic)
;;

Printf.printf "%d\n" (result ());;
