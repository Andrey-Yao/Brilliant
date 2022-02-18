open! Core
open Cfg
open Sig


module Forward: functor(F: Frame) ->
                DataFlow with
                  type t = (F.p * F.p) String.Map.t

module Backward: functor(F: Frame) ->
                 DataFlow with
                   type t = (F.p * F.p) String.Map.t
