
(* This file is free software, part of logf. See file "LICENSE" for more information *)

open Cmdliner

(* Types *)
(* ************************************************************************* *)

type run = {
  log_file : string;
}

type find = {
  log_file : string;
  regexp : Logf.Regexp.path_regexp;
}

type query = {
  log_file : string;
  node_id : int;
  key : string option;
}

type cmd =
  | Run of run
  | Find of find
  | Query of query

(* Converters *)
(* ************************************************************************* *)

let regexp_conv =
  Arg.conv' (Logf.Regexp.parse, Logf.Regexp.print)


(* Logs & debugging *)
(* ************************************************************************* *)

let logs_level = Logs_cli.level ()

let logs_style = Fmt_cli.style_renderer ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level ~all:true level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let bt =
  let doc = "Enable backtraces" in
  Arg.(value & flag & info ["b"] ~doc)

let setup_bt bt =
  if bt then begin
    Sys.catch_break true;
    Printexc.record_backtrace true
  end;
  ()

(* Common args *)
(* ************************************************************************* *)

let log_file =
  let doc = "Input Logf file" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

let node_id =
  let doc = "Id of a node to query" in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"ID" ~doc)

let key =
  let doc = "Key to query (if not provided all keys will be shown)" in
  Arg.(value & pos 2 (some string) None & info [] ~docv:"KEY" ~doc)

let regexp =
  let doc = "Regexp to use to match log paths" in
  Arg.(required & pos 1 (some regexp_conv) None & info [] ~docv:"REGEXP" ~doc)


(* Main term *)
(* ************************************************************************* *)

let run =
  let open Term.Syntax in
  let+ bt
  and+ logs_level
  and+ logs_style
  and+ log_file
  in
  setup_bt bt;
  setup_log logs_style logs_level;
  Run { log_file; }

(* Find term *)
(* ************************************************************************* *)

let find =
  let open Term.Syntax in
  let+ bt
  and+ logs_level
  and+ logs_style
  and+ log_file
  and+ regexp
  in
  setup_bt bt;
  setup_log logs_style logs_level;
  Find { log_file; regexp; }

(* Query term *)
(* ************************************************************************* *)

let query =
  let open Term.Syntax in
  let+ bt
  and+ logs_level
  and+ logs_style
  and+ log_file
  and+ node_id
  and+ key
  in
  setup_bt bt;
  setup_log logs_style logs_level;
  Query { log_file; node_id; key; }

