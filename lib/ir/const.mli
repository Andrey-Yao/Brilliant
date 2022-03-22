open! Core

type t = Int of int | Bool of bool | Float of float
[@@deriving compare, equal, hash, sexp]

val to_string : t -> string
