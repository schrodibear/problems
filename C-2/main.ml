
open Batteries_uni

module Array = struct
  include Array
  let flip_iter f a = iter a f
end

module List = struct
  include List
  let flip_assoc l a = assoc a l
end

module Printing : sig

  type dimensions

  type point

  type _int

  exception Invalid_operation of string

  class virtual canvas : int -> int -> object
    method draw : stamp -> unit
    method subcanvas : int -> int -> int -> int -> subcanvas
    method private virtual point : int -> int -> char -> unit
    method private check_dimensions : dimensions -> bool
    method check_dn : stamp -> int -> unit
  end
  and root_canvas : int -> int -> object
    inherit canvas
    method contents : string
    method private point : int -> int -> char -> unit
  end
  and subcanvas : point -> int -> int -> int -> int -> object
    inherit canvas
    method private point : int -> int -> char -> unit
  end
  and virtual stamp : char -> int -> int -> int -> object
    method setc : char -> unit
    method virtual get_dimensions : dimensions
    method addn : int -> unit
    method move : int -> int -> unit
    method _match : stamp -> unit
    method virtual draw : point -> unit
    method virtual check_dn : _int -> _int -> int -> bool
    method private line :
      (int -> int -> char -> 'a) ->
        int -> int -> int -> int -> int -> unit
    method private rect :
      (int -> int -> char -> 'b) -> int -> int -> int -> int -> unit
  end

  val name_constructor : (string * (char -> int -> int -> int -> stamp)) list

end = struct

  type dimensions = {
    x : int;
    y : int;
    w : int;
    h : int
  }

  let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

  type point = int -> int -> char -> unit

  type _int = int

  exception Invalid_operation of string

  class virtual canvas w h = object (self)
    method private check_dimensions d =
      d.x >=0 && d.y >= 0 && d.x + d.w <= w && d.y + d.h <= h
    method private virtual point : int -> int -> char -> unit
    method draw (obj : stamp) =
      if self#check_dimensions obj#get_dimensions then
        obj#draw self#point
    method subcanvas w h x y =
      if not (self#check_dimensions { x; y; w; h }) then
          raise (Invalid_argument "subcanvas");
      new subcanvas self#point w h x y
    method check_dn : stamp -> int -> unit = fun obj dn ->
      if not (obj#check_dn w h dn) then raise (Invalid_operation "addn")
  end
  and root_canvas w h = object (self)
    inherit canvas w h

    val matrix = Array.make_matrix h w '.'

    method private point x y c =
      matrix.(y).(x) <- c
    method contents =
      let b = Buffer.create 100 in
      let p = Buffer.add_char b in
      Array.flip_iter matrix (fun s ->
        Array.flip_iter s (fun c -> p c);
      p '\n');
      Buffer.contents b
  end
  and subcanvas point w h x y = object
    inherit canvas w h

    method private point x' y' c =
      point (x + x') (y + y') c
  end
  and virtual stamp c n x y = object (self)

    val mutable c = c
    val mutable n = n
    val mutable x = x
    val mutable y = y

    method setc (c' : char) =
      c <- c'
    method virtual check_dn : int -> int -> int -> bool
    method virtual get_dimensions : dimensions
    method addn dn =
      n <- n + dn
    method move dx dy =
      x <- x + dx;
      y <- y + dy
    method _match (o : stamp) =
      let { x = x'; y = y' } = o#get_dimensions in
      x <- x';
      y <- y'
    method private line point n x y dx dy =
      for i = 0 to n - 1 do
        point (x + i * (sign dx)) (y + i * (sign dy)) c
      done
    method private rect point w h x y =
      for i = 0 to h - 1 do
        for j = 0 to w - 1 do
          point (x + j) (y + i) c
        done
      done
    method virtual draw : (int -> int -> char -> unit) -> unit
    method private virtual check_dn : int -> int -> int -> bool
  end

  let name_constructor =
    [ "square", (fun c n x y ->
                  (object (self)
                    inherit stamp c n x y
                    method check_dn w h dn =
                      let n' = n + dn in
                      2 <= n' && n' <= (min w (2 * h))
                    method get_dimensions =
                      { x; y; w = n; h = n / 2 }
                    method draw point =
                      self#line point n x y 1 0;
                      self#line point (n / 2) x y 0 1;
                      self#line point n x (y + n / 2 - 1) 1 0;
                      self#line point (n / 2) (x + n - 1) y 0 1
                   end :> stamp));
      "tile", (fun c n x y ->
                (object (self)
                  inherit stamp c n x y
                  method check_dn w h dn =
                    let n' = n + dn in
                    2 <= n' && n' <= (min w (2 * h))
                  method check_n n' =
                    2 <= n'
                  method get_dimensions =
                    { x; y; w = n; h = n / 2 }
                  method draw point =
                    self#rect point n (n / 2) x y
                 end :> stamp));
    "chess\\", (fun c n x y ->
                 (object (self)
                   inherit stamp c n x y
                   method check_dn w h dn =
                     let n' = n + dn in
                     1 <= n' && n' <= (min (w / 2) (h - 1))
                   method get_dimensions =
                     { x; y; w = 2 * n; h = 2 * ((n + 1) / 2) }
                   method draw point =
                     self#rect point n ((n + 1) / 2) x y;
                     self#rect point n ((n + 1) / 2) (x + n) (y + (n + 1) / 2)
                  end :> stamp));
    "chess/", (fun c n x y ->
                (object (self)
                  inherit stamp c n x y
                  method check_dn w h dn =
                    let n' = n + dn in
                    1 <= n' && n' <= (min (w / 2) (2 * (h / 2)))
                  method get_dimensions =
                    { x; y; w = 2 * n; h = 2 * ((n + 1) / 2) }
                  method draw point =
                    self#rect point n ((n + 1) / 2) (x + n) y;
                    self#rect point n ((n + 1) / 2) x (y + (n + 1) / 2)
                 end :> stamp));
    "xcross", (fun c n x y ->
                (object (self)
                  inherit stamp c n x y
                  method check_dn w h dn =
                    let n' = n + dn in
                    0 <= n' && n' <= (min ((w - 1) / 2) ((h - 1) / 2))
                  method get_dimensions =
                    { x; y; w = 2 * n + 1; h = 2 * n + 1 }
                  method draw point =
                    self#line point (2 * n + 1) x y 1 1;
                    self#line point (2 * n + 1) x (y + 2 * n) 1 (-1)
                end :> stamp));
    "+cross", (fun c n x y ->
                (object (self)
                  inherit stamp c n x y
                  method check_dn w h dn =
                    let n' = n + dn in
                    0 <= n' && n' <= (min ((w - 1) / 4)  ((h - 1) / 2))
                  method get_dimensions =
                    { x; y; w = 4 * n + 1; h = 2 * n + 1 }
                  method draw point =
                    self#line point (4 * n + 1) x (y + n) 1 0;
                    self#line point (2 * n + 1) (x + 2 * n) y 0 1
                end :> stamp))
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
    let set_if_none arr i b = match arr.(i) with
      | None -> arr.(i) <- Some b
      | Some _ -> raise (Invalid_argument "set_if_none")
    in
    let fail () = failwith ("wrong input in line #" ^ (string_of_int !nline)) in
    try
      let elems = read_list () in
      let canvases = Array.make (~%(hd elems) + 1) None in
      let objects = Array.make (~%(nth elems 1) + 1) None in
      let elems = read_list () in
      let root_canvas = new root_canvas ~%(nth elems 1) ~%(nth elems 2) in
      canvases.(~%(hd elems)) <- Some (root_canvas :> canvas);
      let curr_canvas = ref (~%(hd elems)) in
      try while true do
        (try
           (match read_list () with
             | [ "new"; id; "subcanvas"; parent; w; h; x; y ] ->
               set_if_none canvases ~%id
                 ((get canvases.(if parent <> "-1" then ~%parent else !curr_canvas))#
                  subcanvas ~%w ~%h ~%x ~%y :> canvas)
             | [ "new"; id; name; c; n; x; y ] ->
               set_if_none objects ~%id (flip_assoc name_constructor name c.[0] ~%n ~%x ~%y)
             | [ "switch"; id ] ->
               if canvases.(~%id) <> None then
                 curr_canvas := ~%id
             | command :: id :: tail ->
               let obj () = get objects.(~%id) in
               (match command :: tail with
                 | [ "draw" ] -> (get canvases.(!curr_canvas))#draw (obj () :> stamp)
                 | [ "match"; id ] -> (obj ())#_match **> (get objects.(~%id) :> stamp)
                 | [ "move"; dx; dy ] -> (obj ())#move ~%dx ~%dy
                 | [ "setc"; c ] -> (obj ())#setc c.[0]
                 | [ "addn"; dn ] -> root_canvas#check_dn (obj ()) ~%dn; (obj ())#addn ~%dn
                 | _ -> fail ())
             | _ -> fail ());
         with
           | Invalid_argument "subcanvas"
           | Invalid_argument "set_if_none"
           | Not_found -> ())
        done with End_of_file -> print_string root_canvas#contents
    with
      | _ -> fail ()

let generate seed () =
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
      | _ -> failwith "Impossible"
  done

let () =
  let f = ref run in
  Arg.parse
    [ ("-g", Arg.Int (fun seed -> f := generate seed), "Generate test case with the seed specified") ]
    (fun _ -> ())
    "Solution for problem C-2.";
  !f ()
