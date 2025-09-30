
(* This file is free software, part of logf. See file "LICENSE" for more information *)

open Logf

let rec fib n =
  withf "Starting fib(%d)" n
    ~key:(string_of_int n) ~data:[| "n", Int n; |] ~f:(fun () ->
        match n with
        | 0 -> 0
        | 1 -> 1
        | _ -> fib (n - 1) + fib (n - 2)
      )

let main n =
  logf "Initialization...";
  let res = fib n in
  logf "Finished !"
    ~data:[|"res", Int res|];
  ()

let () =
  let n = ref 0 in
  Arg.parse [
  ] (fun anon_arg ->
      n := int_of_string anon_arg)
    {|fib <n> : example of nested logging using the fibonacci function|};
  main !n

