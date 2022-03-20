(*
open! Core
open Ir

module Value : sig
  type t = { op : string; args : string list } [@@deriving equal, sexp]
  type comparator_witness

  val compare : t -> t -> int
  val comparator : (t, comparator_witness) Base.Comparator.t
end

(** The type of canonical values. *)
type canon = Variable of string | Constant of Const.t

type data = {
  val_to_num : int Map.Make(Value).t; (*Row of values*)
  var_to_num : int String.Map.t; (*cloud*)
  num_to_can : canon Int.Map.t; (*Canon*)
}
*)
