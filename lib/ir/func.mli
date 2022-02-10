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

(*
module Dominance : sig
  module type S = sig
    type out

    val dominators : ?strict:bool -> t -> out String.Map.t
    val dominated : ?strict:bool -> t -> out String.Map.t
    val tree : t -> out String.Map.t * out String.Map.t
    val frontier : t -> out String.Map.t
    val back_edges : t -> out String.Map.t
  end

  module Sets : S with type out := String.Set.t
  module Lists : S with type out := string list
end
 *)
