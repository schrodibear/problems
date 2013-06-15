
open Batteries_uni

exception Overflow
exception Underflow

class ['a] buffer = object
  val mutable contents = None
  method put : 'a -> unit = fun sth -> match contents with
    | None -> contents <- Some sth
    | _ -> raise Overflow
  method get = match contents with
    | Some sth -> contents <- None; sth
    | _ -> raise Underflow
end

let () =
  let o = new buffer in
  let q = Queue.create () in
  let f = ref true in
  read_line () |>
  Str.split (Str.regexp_string " ") |>
  List.iter (fun w -> Queue.push w q);
  let p w = print_string ((if !f then (f := false; "") else " ") ^ w) in
  while not (Queue.is_empty q) do
    try
      match Queue.pop q with
        | "put" -> o#put (Queue.pop q)
        | "get" -> p o#get
        | s -> raise (Invalid_argument s)
    with
      | Overflow -> p "!overflow!"
      | Underflow -> p "!underflow!"
  done;
  print_endline "";
