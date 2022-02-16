open! Core
open Dataflow
open Ir

module ReachingDefinitionsF : Frame = struct

  module Strmap = String.Map
  
  type poset = Ir.Instr.t list Strmap.t

  type t = (poset * poset) String.Map.t

  let top = Strmap.empty

  let meet = failwith "unimplemented"

  let equal = Strmap.equal (List.equal Instr.equal)

  (*
  let kills x instr =
    match Instr.dest instr with
    |

    *)
  let transfer x instr = failwith "unimplented"

  let optimize = failwith "unimplemented"
  
end
