
let read format f = Scanf.bscanf Scanf.Scanning.stdib format f

let () =
  let n = read "%d\n" (fun x -> x) in
  let nowhere = -1 in
  let lefts = Array.make n nowhere in
  let rights = Array.make n nowhere in
  for i = 1 to n do
    let a = read "%d " (fun x -> x - 1) in
    if lefts.(a) = nowhere then lefts.(a) <- i;
    rights.(a) <- n - i + 1
  done;
  let left = ref (Int64.of_int 0) in
  let right = ref (Int64.of_int 0) in
  let m = read "%d\n" (fun x -> x) in
  for i = 1 to m do
    let q = read " %d" (fun x -> x - 1) in
    left := Int64.add !left (Int64.of_int lefts.(q));
    right := Int64.add !right (Int64.of_int rights.(q))
  done;
  Printf.printf "%Ld %Ld\n" !left !right 
  
