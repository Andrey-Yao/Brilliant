open Stdio

module type VIngredient = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val to_string: t -> string
  val sexp_of_t: t -> Base.Sexp.t
  val t_of_sexp: Base.Sexp.t -> t 
end

module type EIngredient = sig
  include Base.Sexpable.S
end


module type Common = sig
  type t
  type v

  module VS: Core_kernel.Set_intf.S with type Elt.t = v

  (**[empty] makes an empty graph*)
  val empty: t
  
  (**Adds [n] with no edges to [g]*)
  val add_vert: t -> v -> t
  
  (**Removes [n] and its associated edges from [g]*)
  val del_vert: t -> v -> t

  val vert_lst: t -> v list
  
  val succs: t -> v -> VS.t
  val preds: t -> v -> VS.t

end


module type Unlabelled = sig
  
  include Common

  val full: v list -> t

  (**adds edge [e]. Creates [src] and [dst] nodes if missing*)
  val add_edge: t -> src:v -> dst:v -> t

  val vert_lst: t -> v list

  val bfs: t -> v -> t
  
  (**Generic function for outputting graph to graphviz form.*)
  val to_dot : oc:Out_channel.t -> label:string -> ?nf:(v -> string)
               -> ?ef:(v -> v -> string) -> t -> unit
end


module type Labelled = sig
  
  include Common
  type e
  type edge = e * v
  module ES: Core_kernel.Set_intf.S with type Elt.t = edge

  (**adds edge [e]. Creates [src] and [dst] nodes if missing*)
  val add_edge: t -> src:v -> edg:e -> dst:v -> t
  val succs_e: t -> v -> ES.t
  val preds_e: t -> v -> ES.t

  (**Generic function for outputting graph to graphviz form.*)
  val to_dot : oc:Out_channel.t -> label:string -> ?nf:(v -> string)
               -> ?ef:(v -> e -> v -> string) -> t -> unit
end
