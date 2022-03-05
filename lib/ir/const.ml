(*modified from https://github.com/sampsyo/bril/tree/main/bril-ocaml*)
open! Core
open! Util.Common

type t = Int of int | Bool of bool | Float of float
[@@deriving compare, equal, hash, sexp_of]

let to_string = function
  | Int i -> Int.to_string i
  | Bool b -> Bool.to_string b
  | Float f -> Float.to_string f
