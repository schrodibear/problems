
let () =
  let (@@) f x = f x in
  let (|>) x f = f x in
  let open List in
  let split = Str.split @@ Str.regexp_string " " in
  let n = read_int () in
  let bottles = Array.make n (0, 0) in
  for i = 0 to n - 1 do
    read_line () |> split |> map int_of_string |> fun l_pair ->
    bottles.(i) <- hd l_pair, hd @@ tl l_pair;
  done;
  let can_open i j = i <> j && snd bottles.(j) = fst bottles.(i) in
  let answer = ref 0 in
  for i = 0 to n - 1 do
    try
      for j = 0 to n - 1 do
        if can_open i j then raise Exit
      done;
      incr answer
    with Exit -> ()
  done;
  print_endline @@ string_of_int !answer

