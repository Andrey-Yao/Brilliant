
module G: Util.Sig.Unlabelled with type v = string and type VS.Elt.t = string

type t = G.t

val dominators: Ir.Func.t -> t

val submissive_tree: t -> t

(**Dominance frontier*)
val submissive_frontier: t -> Ir.Func.G.t -> t

val to_dot: oc:Stdio.Out_channel.t -> label:string -> t -> unit
