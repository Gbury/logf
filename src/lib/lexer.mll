
(* This file is free software, part of logf. See file "LICENSE" for more information. *)

(** {1 Messages Lexer} *)

{
  exception Error
  exception End_of_file

  type msg =
    | Start of { id : int; key: string; }
    | Stop of { id : int; }
    | Log of { id : int; }

  type res = {
    pos : int;
    msg : msg;
  }

  let mk_id s =
    try int_of_string s
    with Failure _ -> raise Error

  let ret ~pos ~msg = { pos; msg; }
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+

let k_sep = '\xC2' '\xA0'
let kvp_sep = '\xE2' '\x80' '\xA8'
let msg_sep = '\xE2' '\x80' '\xA9'

rule node = shortest
  | "[^" (integer as id) "]" _* k_sep (_* as key) k_sep _* kvp_sep _* msg_sep '\r'? '\n'
      { let pos = Lexing.lexeme_start lexbuf in
        ret ~pos ~msg:(Start { id = mk_id id; key; }) }
  | "[$" (integer as id) "]" _* msg_sep '\r'? '\n'
      { let pos = Lexing.lexeme_start lexbuf in
        ret ~pos ~msg:(Stop { id = mk_id id; }) }
  | "[" (integer as id) "]" _* msg_sep '\r'? '\n'
      { let pos = Lexing.lexeme_start lexbuf in
        ret ~pos ~msg:(Log { id = mk_id id; }) }
  | eof
      { raise End_of_file }
  | [^ '[']
      { raise Error }

and node_data = shortest
  | "[" _* kvp_sep { node_data_aux [] lexbuf }
  | [^ '[']
      { raise Error }

and node_data_aux acc = shortest
  | (_* as key) k_sep _* k_sep (_* as value) msg_sep
    { (key, value) :: acc }
  | (_* as key) k_sep _* k_sep (_* as value) kvp_sep
    { node_data_aux ((key, value) :: acc) lexbuf }

