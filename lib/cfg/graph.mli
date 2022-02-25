open! Core

(**This is a module representing digraphs*)

type edge = True | False | Jump | Next

type t = {
    succs_map : (edge * string) list String.Map.t;
    preds_map : (edge * string) list String.Map.t;
}

val add_edge : t -> src:string -> dst:string -> edge -> t
(**adds edge [e]. Creates [src] and [dst] nodes if missing*)

val remove : t -> string -> t
val succs_e : t -> string -> (edge * string) list
val preds_e : t -> string -> (edge * string) list
val succs : t -> string -> string list
val preds : t -> string -> string list

(**[empty] makes an empty graph*)
val empty : t
