
open Batteries;

open List;

value solution =

     mapi (fun i a -> (i + 1, a)) %> filter (snd %> (>) 0) %> map fst %> fun l -> (length l, l);

value run lst =
  print_endline @@ "[" ^ (String.concat "; " @@ map string_of_int @@ snd @@ solution lst) ^ "]";

value lst = [-1; -1; 2; 3; -4; -5; -6; 1];

run lst;
