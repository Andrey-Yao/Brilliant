open Ir

val to_ssa : Func.t -> Func.t

val preprocess : Func.t -> Func.t * int
