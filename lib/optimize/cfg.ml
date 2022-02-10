open! Core
open Graph.Persistent


type vertex = {id: string; instrs: (Ir.Instr.t list)}
                [@@deriving compare, equal, hash]
type edge = BrTrue | BrFalse | Default [@@deriving compare]

module Cfg =
  Graph__.Persistent.Digraph.ConcreteBidirectionalLabeled(
      struct type t = vertex[@@deriving compare, equal, hash] end)(
      struct type t = edge [@@deriving compare]
             let default : t = Default end)

include Cfg

(*Originally from https://github.com/sampsyo/bril/tree/main/bril-ocaml*)
let process_instrs instrs =
  let block_name i = sprintf "__b%d" i in
  let (name, block, blocks) =
    List.fold
      instrs
      ~init:(block_name 0, [], [])
      ~f:(fun (name, block, blocks) (instr : Ir.Instr.t) ->
        let add_block block =
          match block with
          | Ir.Instr.Label _ :: _ -> (name, block) :: blocks
          | _ -> (name, Label name :: block) :: blocks
        in
        match instr with
        | Label label ->
          if List.is_empty block then (label, [ instr ], blocks)
          else (label, [ instr ], add_block (List.rev block))
        | Jmp _
        | Br _
        | Ret _ ->
          (block_name (List.length blocks + 1), [], add_block (List.rev (instr :: block)))
        | _ -> (name, instr :: block, blocks))
  in
  let blocks =
    (name, List.rev block) :: blocks
    |> List.rev_filter ~f:(fun (_, block) -> not (List.is_empty block))
  in
  let order = List.map blocks ~f:fst in
  let succs =
    List.mapi blocks ~f:(fun i (name, block) ->
        let next =
          match List.last_exn block with
          | Jmp label -> [ label ]
          | Br (_, l1, l2) -> [ l1; l2 ]
          | Ret _ -> []
          | _ ->
            ( match List.nth blocks (i + 1) with
            | None -> []
            | Some (name, _) -> [ name ] )
        in
        (name, next))
    |> String.Map.of_alist_exn
  in
  (String.Map.of_alist_exn blocks, order, succs)


let from_func (f : Ir.Func.t) =
  let blocks, order, succs = process_instrs f.instructions in
  let vertex_folder g name =
    let instrs = String.Map.find_exn blocks name in
    {id=name; instrs=instrs} |> V.create |> add_vertex g in
  (*let edges_folder g = *)
  let graph = List.fold ~init:empty ~f:vertex_folder in ()(*
  let graph = String.Map.fold ~init:graph ~f:
                (fun key data graph -> match data with)*)
