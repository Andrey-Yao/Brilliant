open! Core
open Cfg


module type Frame = sig
  
  type poset
  type t = (poset * poset) String.Map.t
  val top: poset
  val meet: poset -> poset -> poset
  val equal: poset -> poset -> bool
  val transfer: poset -> Ir.Instr.t list -> poset
  val optimize: Cfg.t -> t -> Cfg.t
end


module type DataFlow = sig
  
  type t
  
  (**[solve funct] is the solution to the dataflow
     equations based on funct and the module
     implementation of [D]*)
  val solve: Cfg.t -> t

  val optimize: Cfg.t -> t -> Cfg.t 
end


module Forward: functor(F: Frame) ->
                DataFlow with
                  type t = F.t

module Backward: functor(F: Frame) ->
                 DataFlow with
                   type t = F.t
