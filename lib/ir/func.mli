(*modified from https://github.com/sampsyo/bril/tree/main/bril-ocaml*)
open! Core
open! Common

type t = {
  name : string;
  args : Instr.dest list;
  ret_type : Bril_type.t option;
  instructions : Instr.t list;
}
[@@deriving compare, equal, sexp_of]

val of_json : Yojson.Basic.t -> t
val to_json : t -> Yojson.Basic.t
val to_string : t -> string
