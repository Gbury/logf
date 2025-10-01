
(* This file is free software, part of logf. See file "LICENSE" for more information *)

(* Midule aliases *)
(* ************************************************************************* *)

module M = Map.Make(Int)

type id = int

type tree = {
  id : id;
  key : string option;
  pos_in_file : int;
  children : forest;
}

and forest = tree Vector.t

type t = {
  root : tree;
  logs : tree Vector.t;
  parents : id Vector.t;
}

(* Accessors *)
(* ************************************************************************* *)

let root { root; _ } = root

let get { logs; root = _; parents = _; } id =
  Vector.get logs id

let parent { parents; logs = _; root = _; } id =
  Vector.get parents id

let id { id; _ } = id
let key { key; _ } = key
let pos_in_file { pos_in_file; _ } = pos_in_file

(* Creation *)
(* ************************************************************************* *)

let create () =
  let root = {
    id = 0;
    key = None;
    pos_in_file = -1;
    children = Vector.create ();
  } in
  let logs = Vector.create () in
  let parents = Vector.create () in
  Vector.add_last logs root;
  Vector.add_last parents root.id;
  { root; logs; parents; }

let mk_node ~t ~id ?key ~pos () : tree =
  assert (id = Vector.length t.logs); (* TODO: better error msg *)
  let node = { id; key; pos_in_file = pos; children = Vector.create (); } in
  Vector.add_last t.logs node;
  Vector.add_last t.parents t.root.id;
  node

let add_children ~index (parent : tree) arr =
  Array.iter (fun child ->
      Vector.set index.parents child.id parent.id
    ) arr;
  Vector.append_array parent.children arr

let add_child ~index (parent : tree) node =
  add_children ~index parent [|node|]


(* Debug printing *)
(* ************************************************************************* *)

let debug_key fmt = function
  | None -> Format.fprintf fmt "<>"
  | Some s -> Format.fprintf fmt "<%s>" s

let rec debug_tree fmt (t : tree) =
  Format.fprintf fmt "@[<v 2>[%d]%a:%a@]"
    t.id debug_key t.key debug_forest t.children

and debug_forest fmt (f: forest) =
  Vector.iter (fun t ->
      Format.fprintf fmt "@ %a" debug_tree t) f

let debug fmt ({ root; logs = _; parents = _; } : t) =
  Format.fprintf fmt "@[<v>%a@]"
    (Vector.print ~pp_sep:Format.pp_print_space debug_tree) root.children

(* Path following *)
(* ************************************************************************* *)

let rec follow_in_tree (t : tree) ~path ~names n =
  if n >= Path.length path then t, names
  else
    follow_in_forest t.children
      ~path ~names:(t.key :: names)
      n (Path.pos path n)

and follow_in_forest (f: forest) ~path ~names n i =
  if i >= Vector.length f then
    assert false (* TODO: error *)
  else
    follow_in_tree (Vector.get f i) ~path ~names (n + 1)

let follow ({ root; logs = _; parents = _; } : t) ~path =
  follow_in_tree root ~path ~names:[] 0


(* Path name of a node *)
(* ************************************************************************* *)

let rec path_name_aux ~index (node: tree) acc =
  if node.id = index.root.id then acc
  else path_name_aux ~index (get index (parent index node.id)) (key node :: acc)

let path_name index node =
  path_name_aux ~index node []

