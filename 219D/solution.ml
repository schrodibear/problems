
let read format f = Scanf.bscanf Scanf.Scanning.stdib format f

module L = struct
  let iter lst f = List.iter f lst
  let map lst f = List.map f lst
end

module A = struct
  let iteri f a = Array.iteri a f
end

let bfs graph n start f =
  let flags = Array.make n false in
  let q = Queue.create () in
  Queue.add start q;
  flags.(start) <- true;
  while not (Queue.is_empty q) do
    let v = Queue.take q in
    L.iter graph.(v) (fun (i, dir) ->
      if not flags.(i) then
        (Queue.add i q;
         f v i dir;
         flags.(i) <- true))
  done

let () =
  let n = read "%d\n" (fun x -> x) in
  let graph = Array.make n [] in
  for i = 1 to n - 1 do
    let f, t = read "%d %d\n" (fun f t -> f - 1, t - 1) in
    graph.(f) <- (t, true) :: graph.(f);
    graph.(t) <- (f, false) :: graph.(t)
  done;
  let sum = ref 0 in
  bfs graph n 0 (fun _ _ dir -> if not dir then incr sum);
  let sums = Array.make n !sum in
  let min_sum = ref !sum in
  bfs graph n 0 (fun f t dir ->
    sums.(t) <- sums.(f) + (if dir then 1 else -1);
    min_sum := min !min_sum sums.(t));
  print_endline (string_of_int !min_sum);
  let b = Buffer.create (10 * n) in
  A.iteri sums (fun i s ->
    if s = !min_sum then Buffer.add_string b (string_of_int (i + 1) ^ " "));
  print_endline (Buffer.sub b 0 (Buffer.length b - 1))
