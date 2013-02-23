
open Batteries_uni

module Printing : sig

  type point

  type kind

  class virtual stamp : kind -> char -> int -> int -> int -> object
    method setc : char -> unit
    method addn : int -> unit
    method move : int -> int -> unit
    method virtual print : point -> unit
    method virtual kind : kind
  end

  class canvas : int -> int -> object
    method draw : stamp -> unit
    method undo : stamp -> unit
    method redo : stamp -> unit
    method contents : string
  end

  val name_constructor : (string * (char -> int -> int -> int -> stamp)) list

end = struct

  open Deque

  type point = int -> int -> char -> unit

  type print = point -> unit

  type kind = string

  type action = { mutable undone : bool;
                  print : print;
                  kind : kind }

  type log = action dq

  type position = { x: int; y : int }

  class virtual stamp (name : kind) c n x y = object

    val mutable c = c
    val mutable n = n
    val mutable x = x
    val mutable y = y

    method setc (c' : char) =
      c <- c'
    method addn dn =
      n <- n + dn
    method move dx dy =
      x <- x + dx;
      y <- y + dy
    method kind = name
    method virtual print : point -> unit
  end

  class canvas w h = object
    val log = ref empty

    method draw : stamp -> unit = fun obj ->
      log := (empty, !log) |> uncurry **> fold_left (fun new_log ({ undone; kind } as act) ->
        if not undone || kind <> obj#kind then snoc new_log act else new_log);
      log := cons { undone = false; print = obj#print; kind = obj#kind } !log
    method undo : stamp -> unit = fun obj ->
      match !log |> find (fun { undone; kind } -> not undone && kind = obj#kind) with
        | Some (_, act) -> act.undone <- true
        | None -> ()
    method redo : stamp -> unit = fun obj ->
      match !log |> find ~backwards:true (fun { undone; kind } -> undone && kind = obj#kind) with
        | Some (_, act) -> act.undone <- false
        | None -> ()
    method contents =
      let matrix = Array.make_matrix h w '.' in
      let point x y c =
        try matrix.(y).(x) <- c
        with Invalid_argument "index out of bounds" -> ()
      in
      (rev !log) |> iter (fun { undone; print } -> if not undone then print point);
      let b = Buffer.create 100 in
      let p = Buffer.add_char b in
      matrix |> Array.iter (fun s ->
        s |> Array.iter(fun c -> p c);
      p '\n');
      Buffer.contents b
  end

  let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

  let line (point : point) c n x y dx dy =
    for i = 0 to n - 1 do
      point (x + i * (sign dx)) (y + i * (sign dy)) c
    done

  let rect (point : point) c w h x y =
    for i = 0 to h - 1 do
      for j = 0 to w - 1 do
        point (x + j) (y + i) c
      done
    done

  let name_constructor =
    let pair (x, f) = (x, f x) in
    [ pair ("square",
             (fun name c n x y ->
               object (self)
                 inherit stamp name c n x y
                 method print =
                   let c, n, x, y = c, n, x, y in
                   (fun point ->
                     line point c n x y 1 0;
                     line point c (n / 2) x y 0 1;
                     line point c n x (y + (n / 2) - 1) 1 0;
                     line point c (n / 2) (x + n - 1) y 0 1)
               end));
      pair ("tile",
            (fun name c n x y ->
               object (self)
                 inherit stamp name c n x y
                 method print =
                   let c, n, x, y= c, n, x, y in
                   (fun point ->
                     rect point c n (n / 2) x y)
                 end));
    pair ("chess\\",
          (fun name c n x y ->
             object (self)
               inherit stamp name c n x y
               method print =
                 let c, n, x, y = c, n, x, y in
                 (fun point ->
                   rect point c n ((n + 1) / 2) x y;
                   rect point c n ((n + 1) / 2) (x + n) (y + (n + 1) / 2))
              end));
    pair ("chess/",
          (fun name c n x y ->
             object (self)
               inherit stamp name c n x y
               method print =
                 let c, n, x, y = c, n, x, y in
                 (fun point ->
                   rect point c n ((n + 1) / 2) (x + n) y;
                   rect point c n ((n + 1) / 2) x (y + (n + 1) / 2))
               end));
    pair ("xcross",
          (fun name c n x y ->
             object (self)
               inherit stamp name c n x y
               method print =
                 let c, n, x, y = c, n, x, y in
                 (fun point ->
                   line point c (2 * n + 1) x y 1 1;
                   line point c (2 * n + 1) x (y + 2 * n) 1 (-1))
               end));
    pair ("+cross",
           (fun name c n x y ->
              object (self)
                inherit stamp name c n x y
                method print =
                  let c, n, x, y = c, n, x, y in
                  (fun point ->
                    line point c (4 * n + 1) x (y + n) 1 0;
                    line point c (2 * n + 1) (x + 2 * n) y 0 1)
                end))
    ]
end

let (*run*) () =
  let open List in
  let open Printing in
  let nline = ref 0 in
  let read_list () =
    read_line () |> (incr nline; Str.split (Str.regexp_string " "))
  in
  let (~%) = int_of_string in
  let fail () = failwith ("wrong input in line #" ^ (string_of_int !nline)) in
  try
    let objects = Hashtbl.create 10 in
    let elems = read_list () in
    let root_canvas = new canvas ~%(nth elems 0) ~%(nth elems 1) in
    try while true do
      match read_list () with
        | id :: command :: tail ->
          let get_obj id = Hashtbl.find objects id in
          (match command :: tail with
            | [ "mkst"; name; c; n; x; y ] ->
              if not (Hashtbl.mem objects id) then
                Hashtbl.add objects id **> assoc name name_constructor c.[0] ~%n ~%x ~%y
            | [ "draw" ] -> root_canvas#draw (get_obj id)
            | [ "move"; dx; dy ] -> (get_obj id)#move ~%dx ~%dy
            | [ "setc"; c ] -> (get_obj id)#setc c.[0]
            | [ "addn"; dn ] -> (get_obj id)#addn ~%dn
            | [ "undo" ] -> root_canvas#undo (get_obj id)
            | [ "redo" ] -> root_canvas#redo (get_obj id)
            | _ -> fail())
         | _ -> fail ()
    done with End_of_file -> print_string root_canvas#contents
  with
    | _ -> fail ()

(*let generate seed () =
  let open Printf in
  let open List in
  Random.init seed;
  let random a b = a + Random.int (b - a + 1) in
  let w = random 10 160 in
  let h = random 10 600 in
  printf "%d %d\n" w h;
  let nobjects = random 1 250 in
  let rotatable = init nobjects (fun _ -> Random.bool ()) in
  let clist = ['~'; '!'; '@'; '#'; '$'; '%'; '^'; '&'; '*'; '('; ')'; '_'; '+' ] in
  let random_elem lst = nth lst (random 0 (length lst - 1)) in
  let random_c () = random_elem clist in
  let random_pos () = random 2 10 in
  let random_int () = random (-100) 100 in
  let print_random_stamp i name_lst =
    printf "%s%s %c %d %d %d" (if i = 0 then "" else " ")
            (random_elem name_lst) (random_c ()) (random_pos ())
            (random_int ()) (random_int ())
  in
  flip_iteri rotatable (fun i -> function
    | true -> print_random_stamp i [ "square"; "tile" ]
    | false -> print_random_stamp i ["chess/"; "chess\\"; "xcross"; "+cross" ]);
  let random_obj () = random 1 (min [10; nobjects]) in
  print_endline "";
  for i = 10 to random 10 1000 do
    let id = random_obj () in
    match random 1 6 with
      | 1 -> printf "%d draw\n" id
      | 2 -> printf "%d match %d\n" id (random_obj ())
      | 3 -> printf "%d move %d %d\n" id (random_int ()) (random_int ())
      | 4 -> printf "%d setc %c\n" id (random_c ())
      | 5 -> printf "%d addn %d\n" id (random_pos ())
      | 6 -> if nth rotatable (id - 1) then printf "%d rotate\n" id
      | _ -> failwith "impossible"
  done

let () =
  let f = ref run in
  Arg.parse
    [ ("-g", Arg.Int (fun seed -> f := generate seed), "Generate test case with the seed specified") ]
    (fun _ -> ())
    "Solution for problem C-1.";
  !f ()
*)
