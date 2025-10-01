
(* This file is free software, part of FTW. See file "LICENSE" for more information *)

let src = Logs.Src.create "logf"

(* Helper functions *)
(* ************************************************************************* *)

let print_data fmt l =
  List.iter (fun (key, value) ->
      Format.fprintf fmt "@ @[<hov 2>%s: %s@]" key value) l

let print_path_names fmt l =
  let pp_sep fmt () = Format.fprintf fmt ":" in
  let pp fmt = function
    | None -> ()
    | Some s -> Format.pp_print_string fmt s
  in
  Format.pp_print_list ~pp_sep pp fmt l

let print_node_and_path_names fmt (node, path_names) =
  Format.fprintf fmt "<%d> %a" (Logf.Index.id node) print_path_names path_names

(* Basic usage *)
(* ************************************************************************* *)

let run (options : Options.run) =
  let index = Logf.Parse.mk_index options.log_file in
  Logs.app ~src (fun k->k "%a" Logf.Index.debug index)

(* Find *)
(* ************************************************************************* *)

let find (options : Options.find) =
  let index = Logf.Parse.mk_index options.log_file in
  let paths = Logf.Regexp.match_ options.regexp index in
  match paths with
  | [] ->
    Logs.app ~src (fun k->k "No paths match the given regular expression")
  | [path] ->
    let node, path_names = Logf.Index.follow index ~path in
    let pos_in_file = Logf.Index.pos_in_file node in
    let data = Logf.Parse.node_data options.log_file pos_in_file in
    Logs.app ~src (fun k->
        k "Found match!@\n@[<v 2>%a:%a@]"
          print_node_and_path_names (node, path_names) print_data data)
  | paths ->
    Logs.app ~src (fun k->
        k "@[<v 2>Found multiple matchs!@ %a@]"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space print_node_and_path_names)
          (List.map (fun path -> Logf.Index.follow index ~path) paths))

(* Query *)
(* ************************************************************************* *)

let query (options : Options.query) =
  let index = Logf.Parse.mk_index options.log_file in
  let node = Logf.Index.get index options.node_id in
  let path_name = Logf.Index.path_name index node in
  let pos_in_file = Logf.Index.pos_in_file node in
  let data = Logf.Parse.node_data options.log_file pos_in_file in
  Logs.app ~src (fun k->
      k "@[<v 2>%a:%a@]"
        print_node_and_path_names (node, path_name) print_data data
    )

(* Main Entrypoint *)
(* ************************************************************************* *)

let () =
  (* Parse CLI options *)
  let info = Cmdliner.Cmd.info ~version:"dev" "logf" in
  let cmd =
    let open Cmdliner in
    Cmd.group ~default:Options.run info [
      Cmd.v (Cmd.info "find") Options.find;
      Cmd.v (Cmd.info "query") Options.query;
    ]
  in
  match Cmdliner.Cmd.eval_value cmd with
  (* Errors *)
  | Error `Parse -> exit Cmdliner.Cmd.Exit.cli_error
  | Error (`Term | `Exn) -> exit Cmdliner.Cmd.Exit.internal_error
  (* Help / Version *)
  | Ok (`Help | `Version) -> exit 0
  (* Options parsed, run the code *)
  | Ok `Ok Options.Run options -> run options
  | Ok `Ok Options.Find options -> find options
  | Ok `Ok Options.Query options -> query options
