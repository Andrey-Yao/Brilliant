open! Core
open Ir


module Ver: sig
  type t = string [@@deriving compare, equal, hash]
end


module Edg: sig
  type t = True | False | Jump | Next [@@deriving compare]
  val default: t
end


module CFG: sig
  include Graph__.Sig.P with type V.t = Ver.t
                         and type V.label = Ver.t
                         and type E.t = Ver.t * Edg.t * Ver.t
                         and type E.label = Edg.t
end

                                            
type t = { graph: CFG.t;(*The control flow graph*)
           args: Instr.dest list;
           blocks: string list;(*Blocks in original order*)
           ret_type: Bril_type.t option;
           func_name: string; (*Name of function this cfg represents*)
           name_to_instrs: Instr.t list String.Map.t;(*yeah*) }


val of_func: Func.t -> t
