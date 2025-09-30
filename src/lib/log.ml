
(* This file is free software, part of logf. See file "LICENSE" for more information *)

(* This file should be standalone as much as possible, to allow for it to be
   copied in other projects when it would not be practical to have a dependency
   on an external library (e.g. the flambda2 compiler) *)

(* Control values *)
(* ************************************************************************* *)

let active = ref false
let log_time = ref false

(* Misc helpers *)
(* ************************************************************************* *)

let print_uchar fmt c =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b c;
  Format.fprintf fmt "%s" (Buffer.contents b)

(* Magic values *)
(* ************************************************************************* *)

(* counter to assign a unique int to each log message *)
let _count = ref 0
let next_id () =
  _count := !_count + 1;
  !_count

(* width of log ids *)
let id_width = 4

(* We somewhat abuse some unicode characters to serve as separtors so that
   the logged messages can be both human-readable, but also somewhat parseable
   later (for automatic processing). The current idea is to use the following
   unicode charcaters:
   - line separator character between key-value pairs inside a given message
   - paragraph separator between log messages ({msg_sep})
   with the hope that those will be printed as regular newlines/spaces by most
   editors, thus being mostly transparent when read by humans, but being a lot
   easier to use for a parser to look for.
*)
let k_sep = Uchar.of_int 0x00A0   (* U+00A0 no-break space *)
let kvp_sep = Uchar.of_int 0x2028 (* U+2028 line separator *)
let msg_sep = Uchar.of_int 0x2029 (* U+2029 paragraph separator *)

let k_sep_str = Format.asprintf "%a" print_uchar k_sep
let kvp_sep_str = Format.asprintf "%a" print_uchar kvp_sep
let msg_sep_str = Format.asprintf "%a" print_uchar msg_sep


(* Messages prefix *)
(* ************************************************************************* *)

let print_prefix fmt () =
  if !log_time then
    let time = Unix.gettimeofday () in
    let tm = Unix.localtime time in
    Format.fprintf fmt "[%d/%d/%d %2d:%2d:%2d]"
      tm.tm_mday (tm.tm_mon + 1) tm.tm_year
      tm.tm_hour tm.tm_min tm.tm_sec

(* Message kinds *)
(* ************************************************************************* *)

type kind =
  | Log
  | Start
  | Stop


(* Key-value pairs *)
(* ************************************************************************* *)

type key = string

type value =
  | Int : int -> value
  | Bool : bool -> value
  | Float : float -> value
  | Any : (Format.formatter -> 'a -> unit) * 'a -> value

let print_v fmt = function
  | Int i -> Format.fprintf fmt "%d" i
  | Bool b -> Format.fprintf fmt "%b" b
  | Float f -> Format.fprintf fmt "%g" f
  | Any (pp, x) -> pp fmt x

let print_kvp fmt (key, value) =
  Format.fprintf fmt "@[<hv 2>%s%s :@ %s@[<hov>%a@]@]"
    key k_sep_str k_sep_str print_v value

let print_kvp_array fmt a =
  a |> Array.iter (fun kvp ->
      Format.fprintf fmt "@,%s%a" kvp_sep_str print_kvp kvp)


(* Log messages *)
(* ************************************************************************* *)

let print_msg_id fmt (kind, n) =
  match kind with
  | Log -> Format.fprintf fmt "[%0*d]" (id_width + 1) n
  | Start -> Format.fprintf fmt "[^%0*d]" id_width n
  | Stop -> Format.fprintf fmt "[$%0*d]" id_width n

let print_msg_key fmt o =
  match o with
  | None -> ()
  | Some s ->
    Format.fprintf fmt "%s%s%s" k_sep_str s k_sep_str

let log_aux fmt ~k ?(data=[||]) ~kind ~id ?key msg =
  let after fmt =
    Format.kfprintf k fmt
      "%a%s@]@."
      print_kvp_array data
      msg_sep_str
  in
  Format.fprintf fmt "@[<v 2>%a%a%a "
    print_msg_id (kind, id)
    print_prefix ()
    print_msg_key key;
  Format.kfprintf after fmt msg


(* High Lever interface *)
(* ************************************************************************* *)

type span = int

let logf ?data msg =
  let id = next_id () in
  log_aux
    Format.err_formatter
    ~kind:Log ~id ?data msg
    ~k:(fun _fmt -> ())

let startf ?data ~key msg =
  let id = next_id () in
  log_aux
    Format.err_formatter
    ~kind:Start ~id ~key ?data msg
    ~k:(fun _fmt -> id)

let stopf id msg =
  log_aux
    Format.err_formatter
    ~kind:Stop ~id msg
    ~k:(fun _fmt -> ())

let withf ?data ~key ~f msg =
  let id = next_id () in
  log_aux
    Format.err_formatter
    ~kind:Start ~id ~key ?data msg
    ~k:(fun _fmt ->
        let res = f () in
        log_aux
          Format.err_formatter
          ~kind:Stop ~id ""
          ~k:(fun _fmt -> res))


