open! Core
open Ir
open Util.Common

module ReachingDefinitionsF = struct

  (**[p] is a map from names of defined [vars] to
   sets of [(block_name, instruction)], where
   [instruction] has [var] as [dest]*)
  type p = SIS.t SM.t

  let top = SM.empty

  let meet p1 p2 =
    SM.fold p1
      ~init:p2
      ~f:(fun ~key ~data acc ->
        match SM.find acc key with
        | None -> SM.add_exn ~key ~data acc
        | Some s ->
            let union = SIS.union data s in
            SM.set ~key ~data:union acc)

  let equal = SM.equal SIS.equal

  let transfer p name block =
    List.foldi block
      ~init:p
      ~f:(fun i map inst ->
        match Instr.dest inst with
        | None -> map
        | Some (key, _) ->
           let data = SIS.singleton (name, i) in
            SM.set ~key ~data map)
end
