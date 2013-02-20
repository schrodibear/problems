
let read format f = Scanf.bscanf Scanf.Scanning.stdib format f

let () =
  let n = read "%d\n" (fun x -> x) in
  let lefts = Array.make n None in
  let rights = Array.make n None in
  for i = 1 to n do
    let a = read "%d " (fun x -> x - 1) in
    if lefts.(a) = None then lefts.(a) <- Some i;
    rights.(a) <- Some (n - i + 1)
  done;
  let some64 = function
    | Some x -> Int64.of_int x
    | None -> failwith "some64: None"
  in
  let left = ref 0L in
  let right = ref 0L in
  let m = read "%d\n" (fun x -> x) in
  for i = 1 to m do
    let q = read " %d" (fun x -> x - 1) in
    left := Int64.(add !left (some64 lefts.(q)));
    right := Int64.(add !right (some64 rights.(q)))
  done;
  Printf.printf "%Ld %Ld\n" !left !right

