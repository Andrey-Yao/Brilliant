open! Core
open Ir

module SM = String.Map



module G: sig
  type edge = True | False | Jump | Next
  type t =
    { succs_map: (edge * string) list SM.t;
      preds_map: (edge * string) list SM.t; }

  (**adds edge [e]. Creates [src] and [dst] nodes if missing*)
  (* val add_edge : t -> ~src:string ~dst:string -> edge -> t *)
  val succs_e : t -> string -> (edge * string) list
  val preds_e : t -> string -> (edge * string) list
  val succs : t -> string -> string list
  val preds : t -> string -> string list
  val empty : t
end


(**[(block_name, instrs)]*)
type block_t = (string * Instr.t Array.t)

type t = { graph: G.t;(*The control flow graph*)
           args: Instr.dest list;
           order: string list;(*Blocks in original order*)
           ret_type: Bril_type.t option;
           func_name: string; (*Name of function this cfg represents*)
           map: block_t String.Map.t;(*yeah*) }


val of_func: Func.t -> t

val to_func: t -> Func.t
