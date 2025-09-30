
(* This file is free software, part of logf. See file "LICENSE" for more information *)

(* Type *)
(* ************************************************************************* *)

include Dynarray

let print ~pp_sep pp fmt t =
  let first = ref true in
  iter (fun x ->
      if not !first then pp_sep fmt ();
      first := false;
      pp fmt x) t

