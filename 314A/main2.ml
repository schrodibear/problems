
let () =
  let (@@) f x = f x in
  let (%) f g x = f @@ g x in
  let (|>) x f = f x in
  let open List in
  let open Num in
  let open Scanf in
  scanf "%d %d\n" @@ fun n k ->
  Array.init n (fun i -> scanf "%d " @@ fun a -> i + 1, a) |> Array.to_list |> fun ratings ->
  let rec loop rev_result acc i n = function
    | (_, ai) as r :: rs when acc -/ (Int ai) */ (Int n -/ Int i) */ (Int i -/ Int 1) </ Int k ->
      loop (r :: rev_result) acc i (n - 1) rs
    | (_, ai) :: rs -> loop rev_result (acc +/ (Int ai) */ (Int i -/ Int 1)) (i + 1) n rs
    | [] -> rev rev_result
  in
  loop [] (Int 0) 1 n ratings |> map (string_of_int % fst) |> iter print_endline
