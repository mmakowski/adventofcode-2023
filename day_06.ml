type race = {
  time: int;
  max_distance: int;
};;

let parse_numbers_str str =
  let num_strs = String.split_on_char ' ' str in
  List.filter_map int_of_string_opt num_strs
;;

let parse_numbers_line f =
  let line = input_line f in
  match String.split_on_char ':' line with
    | _ :: nums_str :: [] -> parse_numbers_str nums_str;
    | _ -> invalid_arg line
;;

let parse_races f =
  let times = parse_numbers_line f in
  let dists = parse_numbers_line f in
  close_in_noerr f;
  let mk_race time max_distance = {time; max_distance} in
  List.map2 mk_race times dists
;;

(**
  dist(t, p) = (t - p) * p = -p^2 + tp

  where t - time for race, p - press time, so to find the number of press times better than
  max_distance (r) we need to find the roots of

  -p^2 + tp - r

  and count the number of integers between them.
*)
let num_winning {time; max_distance} =
  let delta = sqrt (float_of_int (time * time - 4 * max_distance)) in
  let time_float = float_of_int time in
  let lower_float = (time_float -. delta) /. 2.0 in
  let upper_float = (time_float +. delta) /. 2.0 in
  let lower = 
    if Float.is_integer lower_float then 
      (int_of_float lower_float) + 1
    else
      int_of_float (ceil lower_float) 
  in
  let upper = 
    if Float.is_integer upper_float then
      (int_of_float upper_float) - 1
    else
      int_of_float (floor upper_float) 
  in
  upper - lower + 1
;;

let rec process_races acc = function
  | [] -> acc;
  | race::races ->
      let n = num_winning race in
      process_races (acc * n) races
;;

let result () = 
  let f = open_in "input-06.txt" in
  let races = parse_races f in
  process_races 1 races
;;

Printf.printf "%d\n" (result ());;
