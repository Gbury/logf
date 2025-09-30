
(* This file is free software, part of logf. See file "LICENSE" for more information *)

(* Index *)
(* ************************************************************************* *)

let add_node acc node =
  match acc with
  | [] -> assert false (* TODO: better error msg *)
  | parent :: _ -> Index.add_child parent node

let rec parse lexbuf index acc =
  match Lexer.node lexbuf with
  | exception Lexer.End_of_file ->
    acc
  | exception Lexer.Error -> assert false
  | { pos; msg = Log { id }; } ->
    (* Format.eprintf "log(%d) / acc: %a@." id print_acc acc; *)
    let node = Index.mk_node ~t:index ~id ~pos () in
    add_node acc node;
    parse lexbuf index acc
  | { pos = _; msg = Stop { id }; } ->
    (* Format.eprintf "stop(%d) / acc: %a@." id print_acc acc; *)
    begin match acc with
      | [] ->
        Format.eprintf "%d ?@." id;
        assert false (* TODO: error msg *)
      | parent :: acc' ->
        assert (id = parent.id);
        (* TODO: try and go up the stack until we find the parent ? to allow for partial logs *)
        parse lexbuf index acc'
    end
  | { pos; msg = Start { id; key; }; } ->
    (* Format.eprintf "start(%d) / acc: %a@." id print_acc acc; *)
    let node = Index.mk_node ~t:index ~id ~pos ~key () in
    add_node acc node;
    parse lexbuf index (node :: acc)

let mk_index file =
  let in_channel = open_in_bin file in
  let lexbuf = Lexing.from_channel in_channel in
  let index = Index.create () in
  let root = Index.root index in
  match parse lexbuf index [root] with
  | [_root] -> index
  | [] | _ :: _ :: _ -> assert false (* TODO: error msg *)


(* Node data *)
(* ************************************************************************* *)

let node_data file pos =
  let in_channel = open_in_bin file in
  let () = seek_in in_channel pos in
  let lexbuf = Lexing.from_channel in_channel in
  Lexer.node_data lexbuf


