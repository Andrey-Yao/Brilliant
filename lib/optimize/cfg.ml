open! Core


type ver = {id: string; instrs: (Ir.Instr.t list)}
             [@@deriving compare, equal, hash]

type edg = True | False | Next [@@deriving compare]

module Cfg =
  Graph__.Persistent.Digraph.ConcreteBidirectionalLabeled(
      struct type t = ver[@@deriving compare, equal, hash] end)(
      struct type t = edg[@@deriving compare]
             let default : t = Next end)


type succs_annotation = Branch of string * string | Next of string | Leaf

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
          | Jmp label -> Next label
          | Br (_, l1, l2) -> Branch(l1, l2)
          | Ret _ -> Leaf
          | _ ->
            ( match List.nth blocks (i + 1) with
            | None -> Leaf
            | Some (name, _) -> Next name )
        in
        (name, next))
    |> String.Map.of_alist_exn
  in
  (String.Map.of_alist_exn blocks, order, succs)


let from_func (f : Ir.Func.t) : Cfg.t =
  let open Cfg in
  let blocks, order, succs = process_instrs f.instructions in
  let name_to_vertex = fun n ->
    {id=n; instrs=String.Map.find_exn blocks n} in
  let vertex_builder g name =
    name |> name_to_vertex |> V.create |> add_vertex g in
  let edges_builder ~key ~data g = begin
      match data with
    | Leaf -> g
    | Next l -> 
        let src = name_to_vertex key in
        let dst = name_to_vertex l in
        E.create src Next dst |> add_edge_e g
    | Branch (t, f) ->
       let src = name_to_vertex key in
       let dest_t = name_to_vertex t in
       let dest_f = name_to_vertex f in
       let edge_t = E.create src True dest_t in
       let edge_f = E.create src False dest_f in
       add_edge_e (add_edge_e g edge_t) edge_f end in
  let graph = List.fold ~init:empty ~f:vertex_builder order in
  String.Map.fold ~init:graph ~f:edges_builder succs
