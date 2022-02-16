open! Core

module Key : sig
  type t = {op: string; args: string list}
             [@@deriving equal, sexp_of]
  type comparator_witness
  val comparator : (t, comparator_witness) Base.Comparator.t
end

type canon = string

type info = {
    val_to_num : int Map.M(Key).t;(*Row of values*)
    var_to_num : int String.Map.t;(*cloud*)
    num_to_can : canon Int.Map.t;(*Canon*)
  }
