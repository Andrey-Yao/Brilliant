(*modified from https://github.com/sampsyo/bril/tree/main/bril-ocaml*)
open! Core

module Binary : sig
  type t =
    | Add
    | Mul
    | Sub
    | Div
    | Fadd
    | Fmul
    | Fsub
    | Fdiv
    | Eq
    | Lt
    | Gt
    | Le
    | Ge
    | Feq
    | Flt
    | Fgt
    | Fle
    | Fge
    | And
    | Or
  [@@deriving compare, equal, hash, sexp_of]

  val is_op : string -> bool
  val of_string : string -> t
  val to_string : t -> string
  val fold : t -> Const.t -> Const.t -> Const.t
end

module Unary : sig
  type t = Not | Id [@@deriving compare, equal, hash, sexp_of]

  val is_op : string -> bool
  val of_string : string -> t
  val to_string : t -> string
  val fold : t -> Const.t -> Const.t
end
