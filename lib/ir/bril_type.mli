(*modified from https://github.com/sampsyo/bril/tree/main/bril-ocaml*)
open! Core

type t = IntType | BoolType | FloatType | PtrType of t
[@@deriving compare, equal, hash, sexp]

val of_json : Yojson.Basic.t -> t
val of_json_opt : Yojson.Basic.t -> t option
val to_json : t -> Yojson.Basic.t
val to_string : t -> string
