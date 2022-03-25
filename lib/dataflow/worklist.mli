open! Core
open Sig

module Forward (F : Frame) :
  DataFlow with type t = F.p String.Map.t

module Backward (F : Frame) :
  DataFlow with type t = F.p String.Map.t
