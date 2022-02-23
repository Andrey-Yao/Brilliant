open Cfg
open Ir

module type Frame = sig
  type p

  val top : p
  val meet : p -> p -> p
  val equal : p -> p -> bool
  val transfer : p -> block_t -> p
end

module type DataFlow = sig
  type t

  val solve : Cfg.t -> t
  (**[solve funct] is the solution to the dataflow
     equations based on funct and the module
     implementation of [D]*)
end
