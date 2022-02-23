open! Core
open Sig
open Ir
module Strmap = String.Map

module ReachingDefinitionsF : Frame = struct
  type value = { block_name : string; instr_num : int }
  [@@deriving compare, sexp]

  module VS = Set.Make (struct
    type t = value [@@deriving compare, sexp]

    [@@@end]
  end)

  type p = VS.t Strmap.t
  (**[p] is a map from names of defined [vars] to
   sets of [(block_name, instruction)], where
   [instruction] has [var] as [dest]*)

  let top = Strmap.empty

  let meet p1 p2 =
    Strmap.fold ~init:p2
      ~f:(fun ~key ~data acc ->
        match Strmap.find acc key with
        | None -> Strmap.add_exn ~key ~data acc
        | Some s ->
            let union = VS.union data s in
            Strmap.set ~key ~data:union acc)
      p1

  let equal = Strmap.equal VS.equal

  let transfer p block =
    Array.foldi ~init:p
      ~f:(fun i map inst ->
        match Instr.dest inst with
        | None -> map
        | Some (key, _) ->
            let data = VS.singleton { block_name = fst block; instr_num = i } in
            Strmap.set ~key ~data map)
      (snd block)
end

(*

module AvailableCopiesF: Frame = struct


end

                                 *)
