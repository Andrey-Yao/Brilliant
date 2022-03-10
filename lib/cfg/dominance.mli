
module G: Util.Sig.Unlabelled with type v = string and type VS.Elt.t = string

type t = G.t

val dominators: Ir.Func.t -> t

val dominance_tree: string -> t -> t

(**Dominance frontier*)
val dominance_frontier: t -> Ir.Func.t -> t

val to_dot: oc:Stdio.Out_channel.t -> label:string -> t -> unit
