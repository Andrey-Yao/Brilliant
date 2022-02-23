open! Core
module SM = String.Map

type edge = True | False | Jump | Next

type t = {
  succs_map : (edge * string) list SM.t;
  preds_map : (edge * string) list SM.t;
}

val add_edge : t -> src:string -> dst:string -> edge -> t
(**adds edge [e]. Creates [src] and [dst] nodes if missing*)

val remove : t -> string -> t
val succs_e : t -> string -> (edge * string) list
val preds_e : t -> string -> (edge * string) list
val succs : t -> string -> string list
val preds : t -> string -> string list
val empty : t
