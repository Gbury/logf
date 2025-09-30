
(* This file is free software, part of logf. See file "LICENSE" for more information *)

(* Type *)
(* ************************************************************************* *)

type t = int array

let is_empty t = Array.length t = 0

let length t = Array.length t

let pos t n = t.(n)

let root = [| |]

let append t i = Array.append t [| i |]

