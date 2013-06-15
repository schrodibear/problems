
let () =
  let (@@) f x = f x in
  let (%) f g x = f @@ g x in
  let (|>) x f = f x in
  let (|*>) (a, b) f = f a b in
  let open List in
  let open Int64 in
  let split = Str.split @@ Str.regexp_string " " in
  read_line () |> split |> map int_of_string |> fun n_k ->
  let n = hd n_k and k = hd @@ tl n_k in
  read_line () |> split |> map int_of_string |> mapi (fun i a -> i + 1, i + 1, a) |> fun ratings ->
  let d n ratings (_, i, ai) =
     (0L, ratings) |*> fold_left (fun s (_, j, aj) ->
       add s @@
         if j < i then sub (mul (of_int aj) @@ sub (of_int j) 1L) @@ mul (sub (of_int n) @@ of_int i) @@ of_int ai
         else 0L)
  in
  let extract_next n ratings =
    let rec loop rev_head = function
      | r :: rs when d n ratings r < of_int k -> (r, rev_append rev_head rs)
      | r :: rs -> loop (r :: rev_head) rs
      | [] -> raise Not_found
    in
    loop [] ratings
  in
  let rec loop rev_result n rs =
    try let r, rs = extract_next n rs in loop (r :: rev_result) (n - 1) @@ mapi (fun i (o, _, a) -> o, i + 1, a) rs
    with Not_found -> rev rev_result
  in
  loop [] n ratings |> map (string_of_int % (fun (o, _, _) -> o)) |> iter print_endline
