open! Core
module Bril_type = Bril_type
module Const = Const
module Func = Func
module Instr = Instr
module Op = Op

type t = Func.t list

val of_json : Yojson.Basic.t -> t
val to_json : t -> Yojson.Basic.t
val to_dot : verbose:bool -> oc:Out_channel.t -> t -> unit
