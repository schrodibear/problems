
open Batteries_uni

module Array = struct
  include Array
  let flip_iter f a = iter a f
end

module List = struct
  include List
  let flip_assoc l a = assoc a l
end

module Printing = struct

  type dimensions = {
    x : int;
    y : int;
    w : int;
    h : int
  }

  let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

  class virtual proto_canvas w h = object (self)
    method private check_dimensions d =
      d.x >=0 && d.y >= 0 && d.x + d.w < w && d.y + d.h < h
    method virtual point : int -> int -> char -> unit
    method draw (obj : stamp) =
      if self#check_dimensions obj#get_dimensions then
        obj#draw (self :> proto_canvas)
  end
  and canvas w h = object (self)
    inherit proto_canvas w h

    val matrix = Array.make_matrix h w '.'

    method point x y c =
      matrix.(y).(x) <- c
    method contents =
      let b = Buffer.create 100 in
      let p = Buffer.add_char b in
      Array.flip_iter matrix (fun s ->
        Array.flip_iter s (fun c -> p c);
      p '\n');
      Buffer.contents b
  end
  and subcanvas parent w h x y = object
    inherit proto_canvas w h

    method point x' y' c =
      parent#point (x + x') (y + y') c
  end
  and virtual stamp c n x y = object (self : 'a)

    val mutable c = c
    val mutable n = n
    val mutable x = x
    val mutable y = y

    method setc (c' : char) =
      c <- c'
    method private virtual check_n : int -> bool
    method virtual get_dimensions : dimensions
    method addn dn =
      if self#check_n (n + dn) then
        n <- n + dn
    method move dx dy =
      x <- x + dx;
      y <- y + dy
    method _match (o : 'a) =
      let { x = x'; y = y' } = o#get_dimensions in
      x <- x';
      y <- y'
    method private line pc n x y dx dy =
      for i = 0 to n - 1 do
        pc#point (x + i * (sign dx)) (y + i * (sign dy)) c
      done
    method private rect pc w h x y =
      for i = 0 to h - 1 do
        for j = 0 to w - 1 do
          pc#point (x + j) (y + i) c
        done
      done
    method virtual draw : proto_canvas -> unit
  end

  let name_constructor =
    [ "square", (fun c n x y ->
                 object (self)
                   inherit stamp c n x y
                   method check_n n' =
                     2 <= n'
                   method get_dimensions =
                     { x; y; w = n; h = n / 2 }
                   method draw pc =
                     self#line pc n x y 1 0;
                     self#line pc (n / 2) x y 0 1;
                     self#line pc n x (y + n / 2 - 1) 1 0;
                     self#line pc (n / 2) (x + n - 1) y 0 1
                 end);
      "tile", (fun c n x y ->
               object (self)
                 inherit stamp c n x y
                 method check_n n' =
                   2 <= n'
                 method get_dimensions =
                   { x; y; w = n; h = n / 2 }
                 method draw pc =
                   self#rect pc n (n / 2) x y
               end);
    "chess\\", (fun c n x y ->
                object (self)
                  inherit stamp c n x y
                  method check_n n' =
                    1 <= n'
                  method get_dimensions =
                    { x; y; w = 2 * n; h = 2 * ((n + 1) / 2) }
                  method draw pc =
                    self#rect pc n ((n + 1) / 2) x y;
                    self#rect pc n ((n + 1) / 2) (x + n) (y + (n + 1) / 2)
                end);
    "chess/", (fun c n x y ->
               object (self)
                  inherit stamp c n x y
                  method check_n n' =
                    1 <= n'
                  method get_dimensions =
                    { x; y; w = 2 * n; h = 2 * ((n + 1) / 2) }
                  method draw pc =
                    self#rect pc n ((n + 1) / 2) (x + n) y;
                    self#rect pc n ((n + 1) / 2) x (y + (n + 1) / 2)
                end);
    "x", (fun c n x y ->
          object (self)
            inherit stamp c n x y
            method check_n n' =
              0 <= n'
            method get_dimensions =
              { x; y; w = 2 * n + 1; h = 2 * n + 1 }
            method draw pc =
              self#line pc (2 * n + 1) x y 1 1;
              self#line pc (2 * n + 1) x (y + 2 * n) 1 (-1)
          end);
    "+", (fun c n x y ->
          object (self)
            inherit stamp c n x y
            method check_n n' =
              0 <= n'
            method get_dimensions =
              { x; y; w = 4 * n + 1; h = 2 * n + 1 }
            method draw pc =
              self#line pc (4 * n + 1) x (y + n) 1 0;
              self#line pc (2 * n + 1) (x + 2 * n) y 0 1
          end)
 ]
end

let () =
  let open List in
  let open Printing in
    let nline = ref 1 in
    let read_list () =
      read_line () |> (incr nline; Str.split (Str.regexp_string " "))
    in
    let (~%) = int_of_string in
    let set_if_none arr i b = match arr.(i) with
      | None -> arr.(i) <- Some b
      | Some _ -> raise (Invalid_argument "?<-")
    in
    let extract = function
      | Some a -> a
      | None -> raise (Invalid_argument "extract")
    in
    let fail () = failwith ("wrong input in line #" ^ (string_of_int !nline)) in
    try
      let elems = read_list () in
      let canvases = Array.make (~%(hd elems) + 1) None in
      let objects = Array.make (~%(nth elems 1) + 1) None in
      let elems = read_list () in
      let root_canvas = new canvas ~%(nth elems 1) ~%(nth elems 2) in
      canvases.(~%(hd elems)) <- Some (root_canvas :> proto_canvas);
      let curr_canvas = ref (~%(hd elems)) in
      try while true do
        (try
           (match read_list () with
             | [ "new"; id; "subcanvas"; parent; w; h ;x; y ] ->
               set_if_none canvases ~%id (new subcanvas
                 (extract canvases.(if parent <> "-1" then ~%parent else !curr_canvas))
                 ~%w ~%h ~%x ~%y :> proto_canvas)
             | [ "new"; id; name; c; n; x; y ] ->
               set_if_none objects ~%id (flip_assoc name_constructor name c.[0] ~%n ~%x ~%y)
             | [ "switch"; id ] ->
               if canvases.(~%id) <> None then
                 curr_canvas := ~%id
             | command :: id :: tail ->
               let obj = extract objects.(~%id) in
               (match command with
                 | "draw" -> obj#draw (extract canvases.(!curr_canvas))
                 | "match" -> obj#_match **> extract objects.(~% (hd tail))
                 | "move" -> obj#move ~%(hd tail) ~%(nth tail 1)
                 | "setc" -> obj#setc (hd tail).[0]
                 | "addn" -> obj#addn ~%(hd tail)
                 | _ -> fail ())
             | _ -> fail ());
         with
           | Invalid_argument "index out of bounds"
           | Invalid_argument "?<-"
           | Invalid_argument "extract" -> ());
        incr nline
        done with End_of_file -> print_string root_canvas#contents
    with
      |_ -> fail ()
