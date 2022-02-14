(*modified from https://github.com/sampsyo/bril/tree/main/bril-ocaml*)
open! Core
open! Common


type dest = string * Bril_type.t [@@deriving compare, equal, sexp_of]
type label = string [@@deriving compare, equal, sexp_of]
type arg = label


type t =
  | Label of label
  | Const of dest * Const.t
  | Binary of dest * Op.Binary.t * arg * arg
  | Unary of dest * Op.Unary.t * arg
  | Jmp of label
  | Br of arg * label * label
  | Call of dest option * string * arg list
  | Ret of arg option
  | Print of arg list
  | Nop
  | Phi of dest * (label * arg) list
  | Speculate
  | Commit
  | Guard of arg * label
  | Alloc of (dest * arg)
  | Free of arg
  | Store of (arg * arg)
  | Load of (dest * arg)
  | PtrAdd of (dest * arg * arg)
[@@deriving compare, equal, sexp_of]


val dest : t -> dest option
val set_dest : dest -> t -> t option
val args : t -> arg list
val set_args : arg list -> t -> t option
val of_json : Yojson.Basic.t -> t
val to_json : t -> Yojson.Basic.t
val to_string : t -> string
val opcode : t -> string
