
(* This file is free software, part of logf. See file "LICENSE" for more information *)

open Logf

let pp_l fmt l =
  let pp_sep fmt () = Format.fprintf fmt ",@ " in
  Format.fprintf fmt "@[<hov>[%a]@]"
    (Format.pp_print_list ~pp_sep Format.pp_print_int) l

let main n =
  logf "Starting...";
  let l = List.init n (fun i -> i) in
  logf "Finished allocating list" ~data:[|
    "n", Int n;
    "list", Any (pp_l, l);
  |];
  let l = List.map (fun i -> 2 * i) l in
  let sum = List.fold_left (+) 0 l in
  logf "Mapping done" ~data:[|
    "sum", Int sum;
    "list", Any (pp_l, l);
  |];
  logf "Finished"

let () =
  let n = ref 0 in
  Arg.parse [
  ] (fun anon_arg ->
      n := int_of_string anon_arg)
    {|basic <n> : basic example for logging messages|};
  main !n

