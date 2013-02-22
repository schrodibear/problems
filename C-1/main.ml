
open Batteries_uni

module Array = struct
  include Array
  let flip_iter f a = iter a f
end

module List = struct
  include List
  let flip_assoc l a = assoc a l
  let rec roll_by n ?(result=[]) = function
    | [] -> rev result
    | lst -> roll_by n ~result:(take n lst :: result) (drop n lst)
end

module Printing : sig

  type point

  type position

  exception Invalid_operation of string

  class virtual ['a] stamp : char -> int -> int -> int -> object
    method setc : char -> unit
    method addn : int -> unit
    method move : int -> int -> unit
    method _match : _ stamp -> unit
    method virtual draw : point -> unit
    method virtual raw : 'a
    method get_position : position
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
    method private point : int -> int -> char -> unit
  end

  val name_constructor : (string * (char -> int -> int -> int ->
    ([> `Rotatable_stamp of 'a rotatable_stamp
     | `Stuck_stamp of 'a stuck_stamp ] as 'a) stamp)) list

end = struct

  let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

  type point = int -> int -> char -> unit

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
                  method draw point =
                    let w, h = if not rotated then n, n / 2 else n / 2, n in
                      self#rect point w h x y
                 end :> _ stamp));
    "chess\\", (fun c n x y ->
                 (object (self)
                   inherit ['a] stuck_stamp c n x y
                   method draw point =
                     self#rect point n ((n + 1) / 2) x y;
                     self#rect point n ((n + 1) / 2) (x + n) (y + (n + 1) / 2)
                  end :> _ stamp));
    "chess/", (fun c n x y ->
                (object (self)
                  inherit ['a] stuck_stamp c n x y
                  method draw point =
                    self#rect point n ((n + 1) / 2) (x + n) y;
                    self#rect point n ((n + 1) / 2) x (y + (n + 1) / 2)
                 end :> _ stamp));
    "xcross", (fun c n x y ->
                (object (self)
                  inherit ['a] stuck_stamp c n x y
                  method draw point =
                    self#line point (2 * n + 1) x y 1 1;
                    self#line point (2 * n + 1) x (y + 2 * n) 1 (-1)
                end :> _ stamp));
    "+cross", (fun c n x y ->
                (object (self)
                  inherit ['a] stuck_stamp c n x y
                  method draw point =
                    self#line point (4 * n + 1) x (y + n) 1 0;
                    self#line point (2 * n + 1) (x + 2 * n) y 0 1
                end :> _ stamp))
    ]
end

let () =
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
            | [ "draw" ] -> root_canvas#draw (obj :> _ stamp)
            | [ "match"; id ] -> obj#_match **> (get objects.(~%id - 1) :> _ stamp)
            | [ "move"; dx; dy ] -> obj#move ~%dx ~%dy
            | [ "setc"; c ] -> obj#setc c.[0]
            | [ "addn"; dn ] -> obj#addn ~%dn
            | [ "rotate" ] -> (match obj#raw with
              | `Rotatable_stamp obj -> obj#rotate ()
              | _ -> fail ())
            | _ -> fail ())
        | _ -> fail ();
      done with End_of_file -> print_string root_canvas#contents
  with
    | _ -> fail ()

(*let generate seed () =
  let open Printf in
  Random.init seed;
  let random a b = a + Random.int (b - a + 1) in
  let maxc = random 0 100 in
  let maxo = random 0 100 in
  let root = random 0 maxc in
  let w = random 10 160 in
  let h = random 10 1000 in
  printf "%d %d\n" maxc maxo;
  printf "%d %d %d\n" root w h;
  let canvases = Array.make (maxc + 1) false in
  let objects = Array.make (maxo + 1) false in
  let current = ref root in
  canvases.(!current) <- true;
  let clist = ['~'; '!'; '@'; '#'; '$'; '%'; '^'; '&'; '*'; '('; ')'; '_'; '+' ] in
  let for_random_object f =
    let id = random 0 maxo in
    if objects.(id) then f id
  in
  for i = 10 to random 10 1000 do
    match random 1 8 with
      | 1 ->
        let id, pid = random 0 maxc, random 0 maxc in
        if canvases.(if pid = -1 then !current else pid) && not canvases.(id) then (
          canvases.(id) <- true;
          let w, h, x, y = random 1 w, random 1 h, random 0 (w - 1), random 0 (h- 1) in
          printf "new %d subcanvas %d %d %d %d %d\n" id pid w h x y)
      | 2 ->
        let id = random 0 maxo in
        if not objects.(id) then (
          objects.(id) <- true;
          let name = List.nth ["square"; "tile"; "chess/"; "chess\\"; "xcross"; "+cross"] (random 0 5) in
          let c = List.nth clist (random 0 (List.length clist -1)) in
          let n = random 2 (min ((w - 1) / 4) ((h - 1) / 4)) in
          let x, y = random 0 (w - 1), random 0 (h - 1) in
          printf "new %d %s %c %d %d %d\n" id name c n x y)
      | 3 ->
        let id = random 0 maxc in
        if canvases.(id) then
          printf "switch %d\n" id
      | 4 -> for_random_object (fun id -> printf "draw %d\n" id)
      | 5 ->
         let id, oid = random 0 maxo, random 0 maxo in
         if objects.(id) && objects.(oid) then
           printf "match %d %d\n" id oid
      | 6 ->
        for_random_object (fun id ->
          let dx, dy = random (-w) w, random (-h) h in
          printf "move %d %d %d\n" id dx dy)
      | 7 ->
        for_random_object (fun id ->
          let c = List.nth clist (random 0 (List.length clist -1)) in
          printf "setc %d %c\n" id c)
      | 8 ->
        for_random_object (fun id ->
          let dn = random (-((h - 1) / 4)) ((h - 1) / 4) in
          printf "addn %d %d\n" id dn)
      | _ -> failwith "impossible"
  done

let () =
  let f = ref run in
  Arg.parse
    [ ("-g", Arg.Int (fun seed -> f := generate seed), "Generate test case with the seed specified") ]
    (fun _ -> ())
    "Solution for problem C-2.";
  !f ()
*)
