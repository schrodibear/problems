
open Batteries_uni

module Printing : sig

  type point

  type kind

  type _int

  exception Invalid_operation of string

  class virtual stamp : kind -> char -> int -> int -> int -> object
    method setc : char -> unit
    method addn : int -> unit
    method move : int -> int -> unit
    method virtual print : point -> unit
    method virtual kind : kind
    method virtual check_dn : _int -> _int -> int -> bool
  end

  class canvas : int -> int -> object
    method draw : stamp -> unit
    method undo : stamp -> unit
    method redo : stamp -> unit
    method check_dn : stamp -> int -> unit
    method contents : string
  end

  val name_constructor : (string * (char -> int -> int -> int -> stamp)) list

end = struct

  open Deque

  type point = int -> int -> char -> unit

  type print = point -> unit

  type kind = string

  type _int = int

  type action = { mutable undone : bool;
                  print : print;
                  kind : kind }

  type log = action dq

  type position = { x: int; y : int }

  exception Invalid_operation of string

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
    method virtual check_dn : int -> int -> int -> bool
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
    method check_dn : stamp -> int -> unit = fun obj dn ->
      if not (obj#check_dn w h dn) then raise (Invalid_operation "addn")
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
               object
                 inherit stamp name c n x y
                 method check_dn w h dn =
                   let n' = n + dn in
                   2 <= n' && n' <= (min w (2 * h))
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
               object
                 inherit stamp name c n x y
                 method check_dn w h dn =
                   let n' = n + dn in
                   2 <= n' && n' <= (min w (2 * h))
                 method print =
                   let c, n, x, y= c, n, x, y in
                   (fun point ->
                     rect point c n (n / 2) x y)
               end));
    pair ("chess\\",
          (fun name c n x y ->
             object
               inherit stamp name c n x y
               method check_dn w h dn =
                   let n' = n + dn in
                   1 <= n' && n' <= (min (w / 2) (h - 1))
               method print =
                 let c, n, x, y = c, n, x, y in
                 (fun point ->
                   rect point c n ((n + 1) / 2) x y;
                   rect point c n ((n + 1) / 2) (x + n) (y + (n + 1) / 2))
             end));
    pair ("chess/",
          (fun name c n x y ->
             object
               inherit stamp name c n x y
               method check_dn w h dn =
                   let n' = n + dn in
                   1 <= n' && n' <= (min (w / 2) (2 * (h / 2)))
               method print =
                 let c, n, x, y = c, n, x, y in
                 (fun point ->
                   rect point c n ((n + 1) / 2) (x + n) y;
                   rect point c n ((n + 1) / 2) x (y + (n + 1) / 2))
             end));
    pair ("xcross",
          (fun name c n x y ->
             object
               inherit stamp name c n x y
               method check_dn w h dn =
                   let n' = n + dn in
                   0 <= n' && n' <= (min ((w - 1) / 2) ((h - 1) / 2))
               method print =
                 let c, n, x, y = c, n, x, y in
                 (fun point ->
                   line point c (2 * n + 1) x y 1 1;
                   line point c (2 * n + 1) x (y + 2 * n) 1 (-1))
             end));
    pair ("+cross",
           (fun name c n x y ->
              object
                inherit stamp name c n x y
                method check_dn w h dn =
                   let n' = n + dn in
                   0 <= n' && n' <= (min ((w - 1) / 4)  ((h - 1) / 2))
                method print =
                  let c, n, x, y = c, n, x, y in
                  (fun point ->
                    line point c (4 * n + 1) x (y + n) 1 0;
                    line point c (2 * n + 1) (x + 2 * n) y 0 1)
              end))
    ]
end

let run () =
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
            | [ "addn"; dn ] -> root_canvas#check_dn (get_obj id) ~%dn; (get_obj id)#addn ~%dn
            | [ "undo" ] -> root_canvas#undo (get_obj id)
            | [ "redo" ] -> root_canvas#redo (get_obj id)
            | _ -> fail())
         | _ -> fail ()
    done with End_of_file -> print_string root_canvas#contents
  with
    | _ -> fail ()

let generate seed () =
  let open Printf in
  let open List in
  let open Printing in
  Random.init seed;
  let random a b = a + Random.int (b - a + 1) in
  let w = random 10 160 in
  let h = random 10 600 in
  printf "%d %d\n" w h;
  let objects = ref [] in
  let clist = ['~'; '!'; '@'; '#'; '$'; '%'; '^'; '&'; '*'; '('; ')'; '_'; '+' ] in
  let random_elem lst = nth lst (random 0 (length lst - 1)) in
  let random_obj () = random_elem !objects in
  let random_id () = "s" ^ (string_of_int (random 1 1000)) in
  let random_name () = random_elem **> fst **> split name_constructor in
  let random_c () = random_elem clist in
  let random_pos () = random 2 10 in
  let random_int () = random (-100) 100 in
  let print_command = function
      | 1 ->
        let id = random_id () in
        if not (mem id !objects) then begin
          objects := id :: !objects;
          printf "%s mkst %s %c %d %d %d\n" id (random_name ()) (random_c ())
          (random_pos ()) (random_int ()) (random_int ())
        end
      | 2 -> printf "%s draw\n" (random_obj ())
      | 3 -> printf "%s move %d %d\n" (random_obj ()) (random_int ()) (random_int ())
      | 4 -> printf "%s setc %c\n" (random_obj ()) (random_c ())
      | 5 -> printf "%s addn %d\n" (random_obj ()) (random_pos ())
      | 6 -> printf "%s undo\n" (random_obj ())
      | 7 -> printf "%s redo\n" (random_obj ())
      | _ -> failwith "impossible"
  in
  print_command 1;
  for i = 1 to random 1 1000 do
    print_command (random 1 7)
  done

let () =
  let f = ref run in
  Arg.parse
    [ ("-g", Arg.Int (fun seed -> f := generate seed), "Generate test case with the seed specified") ]
    (fun _ -> ())
    "Solution for problem C-3.";
  !f ()
