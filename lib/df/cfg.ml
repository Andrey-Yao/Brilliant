open! Core
open Ir


module SM = String.Map


(** Digraph *)
module G = struct
  
  type edge = True | False | Jump | Next
  
  type t = 
    { succs_map: (edge * string) list SM.t;
      preds_map: (edge * string) list SM.t; }

  (**adds edge [e]. Creates [src] and [dst] nodes if missing*)
  let add_edge g ~src ~dst e =
    let opt_to_lst = function None -> [] | Some l -> l in
    let old_succs = SM.find g.succs_map src |> opt_to_lst in
    let old_preds = SM.find g.preds_map dst |> opt_to_lst in
    let ms = SM.set ~key:src ~data:((e, dst)::old_succs) g.succs_map in
    let mp = SM.set ~key:dst ~data:((e, src)::old_preds) g.preds_map in
    { succs_map = ms; preds_map = mp }
  
  let succs_e g n = SM.find_exn g.succs_map n
  
  let preds_e g n = SM.find_exn g.preds_map n
  
  let succs g n = succs_e g n |> List.map ~f:snd
  
  let preds g n = preds_e g n |> List.map ~f:snd
  
  let empty = { succs_map = SM.empty; preds_map = SM.empty; }
end


type block_t = string * (Instr.t Array.t)

type t = { graph: G.t;(*The control flow graph*)
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
  (**The [curr] returned is reversed*)
  let rec blockify curr rest g =
    let open G in
    let src = name in begin
    match rest with
    | [] -> curr, rest, g
    | Label l as h::_ -> (*fix this case*)
       h::curr, rest, add_edge ~src ~dst:l g Next
    | Jmp l as h::t ->
       h::curr, t, add_edge ~src ~dst:l g Jump
    | Br (_, l1, l2) as h::t ->
       let g1 = add_edge ~src ~dst:l1 g True in
       let g2 = add_edge ~src ~dst:l2 g1 False in
       h::curr, t, g2
    | Ret _ as h::t -> h::curr, t, g
    | h::t -> blockify (h::curr) t g end in
  let order = info.blocks @ [name] in
  let block_rev, rest, g = blockify [] instrs info.graph in
  let arr = Array.of_list_rev block_rev in
  let map1 = String.Map.add_exn ~key:name ~data:(name, arr) info.name_to_instrs in
  rest, { info with blocks = order; name_to_instrs = map1; graph = g }, i1
       

(** Updates [info] recursively *)
let rec process_instrs (instrs, info, i) =
  match instrs with
  | [] -> info
  | _ -> process_instrs (next_block instrs info i)


let of_func (funct: Func.t) =
  let init_info =
    { graph = G.empty;
      args = funct.args;
      blocks = [];
      ret_type = funct.ret_type;
      func_name = funct.name;
      name_to_instrs = String.Map.empty } in
  process_instrs (funct.instructions, init_info, 1)


let to_func (g: t) : Func.t = failwith "TODO"
