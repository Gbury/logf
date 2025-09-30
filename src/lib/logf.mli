
(* This file is free software, part of logf. See file "LICENSE" for more information *)

(* Module aliases *)
(* ************************************************************************* *)

module Parse = Parse
module Index = Index
module Regexp = Regexp

(* Logging *)
(* ************************************************************************* *)

type key = string

type span

type value =
  | Int : int -> value
  | Bool : bool -> value
  | Float : float -> value
  | Any : (Format.formatter -> 'a -> unit) * 'a -> value

val logf :
  ?data:(key * value) array ->
  ('a, Format.formatter, unit, unit) format4 -> 'a

val startf :
  ?data:(key * value) array -> key:key ->
  ('a, Format.formatter, unit, span) format4 -> 'a

val stopf : span ->
  ('a, Format.formatter, unit, unit) format4 -> 'a

val withf :
  ?data:(key * value) array -> key:key -> f:(unit -> 'ret) ->
  ('a, Format.formatter, unit, 'ret) format4 -> 'a
