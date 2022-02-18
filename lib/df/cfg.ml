open! Core
open Ir


module Ver = struct
  type t= string [@@deriving compare, equal, hash]
end


module Edg = struct
  type t = True | False | Jump | Next [@@deriving compare]
  let default : t = Next
end


module CFG =
  Graph__.Persistent.Digraph.ConcreteBidirectionalLabeled(Ver)(Edg)

type block_t = string * (Instr.t Array.t)

type t = { graph: CFG.t;(*The control flow graph*)
           args: Instr.dest list;
           blocks: string list;(*Blocks in original order*)
           ret_type: Bril_type.t option;
           func_name: string; (*Name of function this cfg represents*)
           name_to_instrs: block_t String.Map.t;(*yeah*) }


(** [next_block instrs info i] returns [(instrs1, info1, i1)] where
 [info1] is [info] but with fields updated to include the next block in
 [instrs], and [instrs] are the remaining instructions. [i1] is the
 updated counter of [i], unless the block starts with a label*)
let next_block (instrs: Instr.t list) (info: t) i: Instr.t list * t * int =
  let open Instr in
  let name, i1 = match List.hd_exn instrs with
    | Label l -> l, i
    | _ ->  sprintf "__B%d" i, i + 1 in
  let rec blockify curr rest g =
    let open CFG in
    let src = V.create name in begin
    match rest with
    | [] -> curr, rest, g
    | Label l::_ ->
       let e = E.create src Next (V.create l) in
       curr, rest, add_edge_e g e
    | Jmp l::t ->
       let e = E.create src Jump (V.create l) in
       curr, t, add_edge_e g e
    | Br (_, l1, l2) as h::t ->
       let et = E.create src True (V.create l1) in
       let ef = E.create src False (V.create l2) in
       h::curr, t, add_edge_e (add_edge_e g et) ef
    | Ret _ as h::t ->
       h::curr, t, g
    | h::t ->
       blockify (h::curr) t g end in
  let blocks1 = info.blocks @ [name] in
  let block_rev, rest, g = blockify [] instrs info.graph in
  let length = List.length block_rev in
  let arr = Array.create ~len:length Instr.Nop in
  List.iteri ~f:(fun i a -> Array.set arr (length - i - 1) a) block_rev; 
  let map1 = String.Map.add_exn ~key:name ~data:(name, arr) info.name_to_instrs in
  let infoo = { info with blocks = blocks1; name_to_instrs = map1; graph = g } in
  rest, infoo, i1
       

(** Updates [info] recursively *)
let rec process_instrs (instrs, info, i) =
  match instrs with
  | [] -> info
  | _ -> process_instrs (next_block instrs info i)


let of_func (funct: Func.t) =
  let init_info =
    { graph = CFG.empty;
      args = funct.args;
      blocks = [];
      ret_type = funct.ret_type;
      func_name = funct.name;
      name_to_instrs = String.Map.empty } in
  process_instrs (funct.instructions, init_info, 1)


(*
let to_func (g: t) : Func.t =
  let block_to_instr = 
    *)
