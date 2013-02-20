
open Batteries_uni

module String = struct
  include String

  let flip_iteri s f = iteri f s
end

let bitset_of_string s =
  let result = BitSet.create (String.length s) in
  String.flip_iteri s (fun i c -> if c = '1' then BitSet.set result i);
  result

let string_of_bitset l bs =
  let result = String.create l in
  String.flip_iteri result (fun i _ ->
    result.[i] <- if BitSet.is_set bs i then '1' else '0');
  result

let make_name_function l bs =
  let result = Hashtbl.create 8 in
  let toggle = BitSet.toggle bs
  and get = BitSet.is_set bs
  and put = BitSet.put bs
  and unset = BitSet.unset bs
  in
  let rsh () =
    let mem = get (l - 1) in
    for i = l - 1 downto 1 do put (get (i - 1)) i done;
    put mem 0
  in
  let unseti f =
    for i = 0 to l - 1 do if f i then unset (l - i - 1) done
  in
  List.iter (fun (a, b) -> Hashtbl.add result a b)
    [ "eql", (fun () -> ());
      "not", (fun () -> for i = 0 to l - 1 do toggle i done);
      "inc", (fun () ->
        let rec loop pos =
          toggle pos;
          if pos > 0 && not (get pos) then loop (pos - 1)
        in
        loop (l - 1));
      "lsh", (fun () ->
        let mem = get 0 in
        for i = 0 to l - 2 do put (get (i + 1)) i done;
        put mem (l - 1));
      "rsh", rsh;
      "ssh", (fun () ->
        rsh ();
        put (get (1 mod l)) 0);
      "zeb", (fun () -> unseti (fun i -> i mod 2 = 0));
      "zob", (fun () -> unseti (fun i -> i mod 2 <> 0)) ];
  result

let () =
  let commands = read_line () in
  let (length, bit_set) = (String.length &&& bitset_of_string) (read_line ()) in
  let name_function = make_name_function length bit_set in
  Hashtbl.add name_function "stop" (fun () -> ());
  let program =
    Str.split (Str.regexp_string " ") commands |>
    List.fold_left
      (fun f command -> (fun () ->
        f ();
        (Hashtbl.find name_function command) ()))
      (fun () -> ())
  in
  program ();
  print_endline (string_of_bitset length bit_set)
