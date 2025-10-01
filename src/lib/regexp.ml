
(* This file is free software, part of logf. See file "LICENSE" for more information *)

(* Type *)
(* ************************************************************************* *)

(* We currently support only exact matches for string regexps *)
type string_regexp =
  | Exact of string

(* Note: this is not a good or realistic implementation of correct regexps. *)
(* We may want a type for "normal" regepx on strings, and one for regepxs
   on paths, which may include regexps on strings for intermediate nodes. *)
type path_regexp =
  | Any_one_node
  | Node of string_regexp
  | Plus of path_regexp
  | Star of path_regexp
  | Seq of path_regexp list

let rec print fmt = function
  | Any_one_node -> Format.fprintf fmt "*"
  | Star Any_one_node -> Format.fprintf fmt "**"
  | Node Exact s -> Format.fprintf fmt "%s" s
  | Plus t -> Format.fprintf fmt "%a+" print t
  | Star t -> Format.fprintf fmt "(%a)*" print t
  | Seq l ->
    let pp_sep fmt () = Format.fprintf fmt ":" in
    Format.pp_print_list ~pp_sep print fmt l

let parse s =
  let l = String.split_on_char ':' s in
  Ok (Seq (List.map (function
      | "*" -> Any_one_node
      | "**" -> Star Any_one_node
      | s -> Node (Exact s)
    ) l))


(* Regepx compilation/matching *)
(* ************************************************************************* *)

let match_ (_regexp: path_regexp) (_index : Index.t) : Path.t list =
  (* TODO: implement automata determinization for regexp and use it *)
  []

