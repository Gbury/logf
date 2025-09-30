
(* This file is free software, part of logf. See file "LICENSE" for more information *)

(* Type *)
(* ************************************************************************* *)

(* Note: this is not a good or realistic implementation of correct regexps. *)
(* We may want a type for "normal" regepx on strings, and one for regepxs
   on paths, which may include regexps on strings for intermediate nodes. *)
type t =
  | Empty
  | Any_one_node
  | Node of string
  | Plus of t
  | Star of t
  | Seq of t * t

let rec print fmt = function
  | Empty -> Format.fprintf fmt "" (* TODO: do something ? *)
  | Any_one_node -> Format.fprintf fmt "*"
  | Star Any_one_node -> Format.fprintf fmt "**"
  | Node s -> Format.fprintf fmt "%s" s
  | Plus t -> Format.fprintf fmt "%a+" print t
  | Star t -> Format.fprintf fmt "(%a)*" print t
  | Seq (t, t') -> Format.fprintf fmt "%a:%a" print t print t'

let parse s =
  let l = String.split_on_char ':' s in
  let rec aux = function
    | [] -> Ok Empty
    | x :: r ->
      match aux r with
      | Error _ as res -> res
      | Ok t ->
        let t' =
          match x with
          | "*" -> Any_one_node
          | "**" -> Star Any_one_node
          | s -> Node s
        in
        Ok (Seq (t', t))
  in
  aux l


(* Regepx compilation/matching *)
(* ************************************************************************* *)

let match_ (_regexp: t) (_index : Index.t) : Path.t list =
  (* TODO: implement automata determinization for regexp and use it *)
  []

