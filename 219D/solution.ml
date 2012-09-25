
let read format f = Scanf.bscanf Scanf.Scanning.stdib format f

let bfs graph start f =
  let flags = Array.make (Array.length graph) false in
  let q = Queue.create () in
  let fold_vertex result v =
    List.fold_left (fun result (i, dir) ->
      if not flags.(i) then (Queue.add i q; flags.(i) <- true; f result v i dir)
      else result) result graph.(v)
  in
  let rec main_loop result = match Queue.is_empty q with
    | true -> result
    | false -> main_loop (fold_vertex result (Queue.take q))
  in
  Queue.add start q;
  flags.(start) <- true;
  main_loop

let () =
  let n = read "%d\n" (fun x -> x) in
  let graph = Array.make n [] in
  for i = 1 to n - 1 do
    let f, t = read "%d %d\n" (fun f t -> f - 1, t - 1) in
    graph.(f) <- (t, true) :: graph.(f);
    graph.(t) <- (f, false) :: graph.(t)
  done;
  let sum = bfs graph 0 (fun sum _ _ dir -> if dir then sum else sum + 1) 0 in
  let sums = Array.make n sum in
  let min_sum = bfs graph 0 (fun min_sum f t dir ->
    sums.(t) <- sums.(f) + (if dir then 1 else -1);
    min min_sum sums.(t)) sum
  in
  print_endline (string_of_int min_sum);
  let b = Buffer.create (10 * n) in
  Array.iteri (fun i s ->
    if s = min_sum then Buffer.add_string b (string_of_int (i + 1) ^ " ")) sums;
  print_endline (Buffer.sub b 0 (Buffer.length b - 1))
