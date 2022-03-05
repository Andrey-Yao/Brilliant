

type t

val dominators: Cflow.t -> t

val bfs: string list -> t -> t

(*
val to_dot: oc:Stdio.Out_channel.t -> nodes:(string list) ->
             t -> unit
                  *)
