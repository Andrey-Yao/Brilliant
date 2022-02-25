open! Core
open Ir
module SM = String.Map
module G = Graph

type block_t = string * Instr.t Array.t
(**[(block_name, instrs)]*)

(**[Next] is fall through*)
type edge = True | False | Jump | Next

type t = {
  graph : edge G.t; (*The control flow graph*)
  args : Instr.dest list;
  order : string list; (*Blocks in original order*)
  ret_type : Bril_type.t option;
  func_name : string; (*Name of function this cfg represents*)
  map : block_t String.Map.t; (*yeah*)
}

val of_func : Func.t -> t
val to_func : t -> Func.t
val to_dot : names_only:bool -> Out_channel.t -> t -> unit
