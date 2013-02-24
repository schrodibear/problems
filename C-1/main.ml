
open Batteries_uni

module Array = struct
  include Array
  let flip_iter f a = iter a f
end

module List = struct
  include List
  let flip_assoc l a = assoc a l
  let flip_iter f a = iter a f
  let flip_iteri f a = iteri a f
  let rec roll_by n ?(result=[]) = function
    | [] -> rev result
    | lst -> roll_by n ~result:(take n lst :: result) (drop n lst)
end

module Printing : sig

  type point

  type position

  type _int

  exception Invalid_operation of string

  class virtual ['a] stamp : char -> int -> int -> int -> object
    method setc : char -> unit
    method addn : int -> unit
    method move : int -> int -> unit
    method _match : _ stamp -> unit
    method virtual draw : point -> unit
    method virtual raw : 'a
    method get_position : position
    method virtual check_dn : _int -> _int -> int -> bool
    method private line : point -> int -> int -> int -> int -> int -> unit
    method private rect : point -> int -> int -> int -> int -> unit
  end

  class virtual ['a] rotatable_stamp : char -> int -> int -> int -> object
    constraint 'a = [> `Rotatable_stamp of _ rotatable_stamp]
    inherit ['a] stamp

    method rotate : unit -> unit
    method raw : 'a
  end

  class virtual ['a] stuck_stamp : char -> int -> int -> int -> object
    constraint 'a = [> `Stuck_stamp of _ stuck_stamp]
    inherit ['a] stamp

    method raw : 'a
  end

  class canvas : int -> int -> object
    method draw : 'a. 'a stamp -> unit
    method contents : string
    method check_dn : 'a .'a stamp -> int -> unit
    method private point : int -> int -> char -> unit
  end

  val name_constructor : (string * (char -> int -> int -> int ->
    ([> `Rotatable_stamp of 'a rotatable_stamp
     | `Stuck_stamp of 'a stuck_stamp ] as 'a) stamp)) list

end = struct

  let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

  type point = int -> int -> char -> unit

  type _int = int

  type position = { x: int; y : int }

  exception Invalid_operation of string

  let counter = ref 0

  class virtual ['a] stamp c n x y = object (self)

    val mutable c = c
    val mutable n = n
    val mutable x = x
    val mutable y = y

    initializer
      incr counter;
      if !counter > 10 then raise (Invalid_operation "new")

    method setc (c' : char) =
      c <- c'
    method addn dn =
      n <- n + dn
    method move dx dy =
      x <- x + dx;
      y <- y + dy
    method _match (o : 'b stamp) =
      let { x = x'; y = y' } = o#get_position in
      x <- x';
      y <- y'
    method get_position = { x; y }
    method private line (point : point) n x y dx dy =
      for i = 0 to n - 1 do
        point (x + i * (sign dx)) (y + i * (sign dy)) c
      done
    method private rect (point : point) w h x y =
      for i = 0 to h - 1 do
        for j = 0 to w - 1 do
          point (x + j) (y + i) c
        done
      done
    method virtual draw : point -> unit
    method virtual check_dn : int -> int -> int -> bool
    method virtual raw : 'a
  end

  class virtual ['a] rotatable_stamp c n x y = object (self)
    inherit ['a] stamp c n x y

    val mutable rotated = false
    method rotate () = rotated <- not rotated
    method raw = `Rotatable_stamp (self :> _ rotatable_stamp)
  end

  class virtual ['a] stuck_stamp c n x y = object (self)
    inherit ['a] stamp c n x y
    method raw = `Stuck_stamp (self :> _ stuck_stamp)
  end

  class canvas w h = object (self)
    val matrix = Array.make_matrix h w '.'

    method private point x y c =
      try matrix.(y).(x) <- c with Invalid_argument "index out of bounds" -> ()
    method draw : 'a. 'a stamp -> unit = fun obj ->
        obj#draw self#point
    method check_dn : 'a . 'a stamp -> int -> unit = fun obj dn ->
      if not (obj#check_dn w h dn) then raise (Invalid_operation "addn")
    method contents =
      let b = Buffer.create 100 in
      let p = Buffer.add_char b in
      Array.flip_iter matrix (fun s ->
        Array.flip_iter s (fun c -> p c);
      p '\n');
      Buffer.contents b
  end

  let name_constructor =
    [ "square", (fun c n x y ->
                  (object (self)
                    inherit ['a] rotatable_stamp c n x y
                    method check_dn w h dn =
                      let n' = n + dn in
                      2 <= n' && n' <= (min w (2 * h))
                    method draw point =
                      let w, h = if not rotated then n, n / 2 else n / 2, n in
                      self#line point w x y 1 0;
                      self#line point h x y 0 1;
                      self#line point w x (y + h - 1) 1 0;
                      self#line point h (x + w - 1) y 0 1
                   end :> _ stamp));
      "tile", (fun c n x y ->
                (object (self)
                  inherit ['a] rotatable_stamp c n x y
                  method check_dn w h dn =
                    let n' = n + dn in
                    2 <= n' && n' <= (min w (2 * h))
                  method draw point =
                    let w, h = if not rotated then n, n / 2 else n / 2, n in
                      self#rect point w h x y
                 end :> _ stamp));
    "chess\\", (fun c n x y ->
                 (object (self)
                   inherit ['a] stuck_stamp c n x y
                    method check_dn w h dn =
                      let n' = n + dn in
                      1 <= n' && n' <= (min (w / 2) (h - 1))
                    method draw point =
                      self#rect point n ((n + 1) / 2) x y;
                      self#rect point n ((n + 1) / 2) (x + n) (y + (n + 1) / 2)
                  end :> _ stamp));
    "chess/", (fun c n x y ->
                (object (self)
                  inherit ['a] stuck_stamp c n x y
                  method check_dn w h dn =
                    let n' = n + dn in
                    1 <= n' && n' <= (min (w / 2) (2 * (h / 2)))
                  method draw point =
                    self#rect point n ((n + 1) / 2) (x + n) y;
                    self#rect point n ((n + 1) / 2) x (y + (n + 1) / 2)
                 end :> _ stamp));
    "xcross", (fun c n x y ->
                (object (self)
                  inherit ['a] stuck_stamp c n x y
                  method check_dn w h dn =
                    let n' = n + dn in
                    0 <= n' && n' <= (min ((w - 1) / 2) ((h - 1) / 2))
                   method draw point =
                     self#line point (2 * n + 1) x y 1 1;
                     self#line point (2 * n + 1) x (y + 2 * n) 1 (-1)
                end :> _ stamp));
    "+cross", (fun c n x y ->
                (object (self)
                  inherit ['a] stuck_stamp c n x y
                  method check_dn w h dn =
                    let n' = n + dn in
                    0 <= n' && n' <= (min ((w - 1) / 4)  ((h - 1) / 2))
                  method draw point =
                    self#line point (4 * n + 1) x (y + n) 1 0;
                    self#line point (2 * n + 1) (x + 2 * n) y 0 1
                end :> _ stamp))
    ]
end

let run () =
  let open List in
  let open Option in
  let open Printing in
  let nline = ref 0 in
  let read_list () =
    read_line () |> (incr nline; Str.split (Str.regexp_string " "))
  in
  let (~%) = int_of_string in
  let fail () = failwith ("wrong input in line #" ^ (string_of_int !nline)) in
  try
    let objects = Array.make 10 None in
    let elems = read_list () in
    let root_canvas = new canvas ~%(nth elems 0) ~%(nth elems 1) in
    read_list () |>
    roll_by 5 |>
    iteri (fun i -> function
      | [ name; c; n; x; y ] -> (try
        objects.(i) <- Some (flip_assoc name_constructor name c.[0] ~%n ~%x ~%y)
        with Invalid_operation "new" -> ())
      | _ -> failwith "impossible");
    try while true do
      match read_list () with
        | id :: command :: tail ->
          let obj = get objects.(~%id - 1) in
          (match command :: tail with
            | [ "draw" ] -> root_canvas#draw obj
            | [ "match"; id ] -> obj#_match **> (get objects.(~%id - 1))
            | [ "move"; dx; dy ] -> obj#move ~%dx ~%dy
            | [ "setc"; c ] -> obj#setc c.[0]
            | [ "addn"; dn ] -> root_canvas#check_dn obj ~%dn; obj#addn ~%dn
            | [ "rotate" ] -> (match obj#raw with
              | `Rotatable_stamp obj -> obj#rotate ()
              | _ -> fail ())
            | _ -> fail ())
        | _ -> fail ();
    done with End_of_file -> print_string root_canvas#contents
  with
    | _ -> fail ()

let generate seed () =
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
