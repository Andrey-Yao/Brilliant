open! Core
open Util

(**[Next] is fall through*)
type edge_lbl = True | False | Jump | Next

(**[G] is the module of the CFG*)
module G: Sig.Labelled
       with type v = string
        and type e = edge_lbl

(**[(block_name, instrs)]*)
type block_t = string * Instr.t Array.t

type t = {
    map : block_t String.Map.t; (*yeah*)
    args : Instr.dest list;
    name : string; (*Name of function*)
    graph : G.t; (*The control flow graph*)
    order : string list; (*Blocks in original or;der*)
    ret_type : Bril_type.t option;
  }

(**Parses a json file*)
val of_json : Yojson.Basic.t -> t

(**Outputs to json file*)
val to_json : t -> Yojson.Basic.t

val to_dot : names_only:bool -> Out_channel.t -> t -> unit

val map_blocks : f:(block_t -> block_t) -> t -> t

(**Cleans [graph] and purges nodes unreachable from entry*)
val clean : t -> t
