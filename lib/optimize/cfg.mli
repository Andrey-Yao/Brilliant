(*
open! Core

type ver = {id: string; instrs: (Ir.Instr.t list)}
                [@@deriving compare, equal, hash]
type edg = True | False | Next [@@deriving compare]

module Cfg : sig include Graph__.Persistent.S end
             *)
