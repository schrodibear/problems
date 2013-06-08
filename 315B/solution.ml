
let (+) = Int64.add
let (-) = Int64.sub

type state = {
    arr : int64 array;
    mutable inc : int64
}

let assign state v x =
  state.arr.(v) <- (x - state.inc)

let increment state y =
  state.inc <- state.inc + y

let print state q =
  print_endline (Int64.to_string (state.arr.(q) + state.inc))

let () =
  let open Pervasives in
  let split_by_spaces = Str.split (Str.regexp_string " ") in
  let n, m = match split_by_spaces (read_line ()) with
    | [n; m] -> int_of_string n, int_of_string m
    | _ -> failwith "Error on line 1"
  in
  let int_list_of_string_list = List.map int_of_string in
  let int64_list_of_string_list = List.map Int64.of_string in
  let state =
    { arr = Array.of_list (int64_list_of_string_list (split_by_spaces (read_line ())));
      inc = 0L }
  in
  assert (Array.length state.arr = n);
  let k = ref 0 in
  try while true do
    (match int_list_of_string_list (split_by_spaces (read_line ())) with
      | [1; v; x] -> assign state (v - 1) (Int64.of_int x)
      | [2; y]    -> increment state (Int64.of_int y)
      | [3; q]    -> print state (q - 1)
      | _ -> failwith ("Error on line " ^ (string_of_int (!k + 2))));
    k := !k + 1
  done with End_of_file -> assert (!k = m)


