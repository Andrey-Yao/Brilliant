open! Core

(**This is a module representing digraphs*)

type 'a t = {
    succs_map : ('a * string) list String.Map.t;
    preds_map : ('a * string) list String.Map.t;
}

(**adds edge [e]. Creates [src] and [dst] nodes if missing*)
val add_edge : 'a t -> src:string -> dst:string -> 'a -> 'a t

val remove : 'a t -> string -> 'a t
val succs_e : 'a t -> string -> ('a * string) list
val preds_e : 'a t -> string -> ('a * string) list
val succs : 'a t -> string -> string list
val preds : 'a t -> string -> string list

(**[empty] makes an empty graph*)
val empty : 'a t

(**Generic function for outputting graph to graphviz form.*)
val to_dot : oc:Out_channel.t ->  nodes:(string list) ->
             label:string -> ?nf:(string -> string)
             -> ?ef:(string -> 'a -> string -> string)
             -> 'a t -> unit
