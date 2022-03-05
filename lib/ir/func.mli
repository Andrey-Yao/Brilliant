open! Core
open Util

(**[Next] is fall through*)
type edge_lbl = True | False | Jump | Next

module G: Sig.Labelled
       with type v = string
        and type e = edge_lbl

type block_t = string * Instr.t Array.t
(**[(block_name, instrs)]*)

type t = {
    map : block_t String.Map.t; (*yeah*)
    args : Instr.dest list;
    name : string; (*Name of function*)
    graph : G.t; (*The control flow graph*)
    order : string list; (*Blocks in original or;der*)
    ret_type : Bril_type.t option;
  }

val of_json : Yojson.Basic.t -> t
val to_json : t -> Yojson.Basic.t
val to_dot : names_only:bool -> Out_channel.t -> t -> unit
val map_blocks : f:(block_t -> block_t) -> t -> t
