(**
#use "day_05.ml";;
*)

type entry = {
  src_start: int;
  dest_start: int;
  length: int;
};;

module Entry = struct
  type t = entry

  let compare e1 e2 =
    compare e1.src_start e2.src_start
end;;

let parse_nums str =
  let num_strs = String.split_on_char ' ' str in
  List.filter_map int_of_string_opt num_strs
;;

let parse_seeds f =
  let line = input_line f in
  match String.split_on_char ':' line with
    | _ :: seed_nums :: [] -> parse_nums seed_nums;
    | other -> invalid_arg line
;;

let parse_entry str =
  match String.split_on_char ' ' str with
    | dest :: src :: len :: [] -> {
          src_start = int_of_string src;
          dest_start = int_of_string dest;
          length = int_of_string len;
        };
    | other -> invalid_arg str
;;

let rec parse_map f map =
  let sorted m = List.sort Entry.compare m in
  try
    let line = String.trim (input_line f) in
    if String.length line = 0 then
      sorted map, true
    else if String.contains line ':' then
      parse_map f map
    else
      let entry = parse_entry line in
      parse_map f (entry::map)
  with End_of_file -> 
    close_in_noerr f;
    sorted map, false
;;

let rec parse_maps f maps =
  match parse_map f [] with
    | [], _ -> List.rev maps;
    | map, false -> List.rev (map::maps);
    | map, true -> parse_maps f (map::maps)
;;

let parse_input f =
  let seeds = parse_seeds f in
  let _ = input_line f in
  let maps = parse_maps f [] in
  seeds, maps
;;

let rec apply_map src = function
  | [] -> src;
  | {src_start; dest_start; length} :: entries ->
      if src < src_start then
        src
      else if src < src_start + length then
        let offset = src - src_start in
        dest_start + offset
      else
        apply_map src entries
;;

let map_chain maps seed = 
  List.fold_left apply_map seed maps
;;

let result () = 
  let f = open_in "input-05.txt" in
  let seeds, maps = parse_input f in
  let locs = List.map (map_chain maps) seeds in
  List.fold_left min Int.max_int locs
;;

Printf.printf "%d\n" (result ());;
