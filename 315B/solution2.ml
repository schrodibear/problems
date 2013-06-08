
let () =
  let (@@) f x = f x in
  let (|>) x f = f x in
  let open List in
  let open Int64 in
  let split_by_spaces = Str.split @@ Str.regexp_string " " in
  read_line () |> split_by_spaces |> map int_of_string |> tl |> hd |> fun m ->
  read_line () |> split_by_spaces |> map of_string |> Array.of_list |> fun arr ->
  let inc = ref 0L in
  for k = 1 to m do
    read_line () |> split_by_spaces |> map int_of_string |> function
      | [1; v; x] -> arr.(v - 1) <- sub (of_int x) !inc
      | [2; y]    -> inc := add !inc @@ of_int y
      | [3; q]    -> print_endline @@ to_string @@ add arr.(q - 1) !inc
      | _ -> failwith @@ "Error on line " ^ string_of_int @@ k + 2
  done


