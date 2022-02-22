open! Core
open Ir


module SM = String.Map


(** Digraph *)
module G = struct

  (**[Next] is fall through*)
  type edge = True | False | Jump | Next
  
  type t = 
    { succs_map: (edge * string) list SM.t;
      preds_map: (edge * string) list SM.t; }

  let empty = { succs_map = SM.empty; preds_map = SM.empty; }
  
  let opt_to_lst = function None -> [] | Some l -> l
  
  let succs_e g n = SM.find g.succs_map n |> opt_to_lst
  
  let preds_e g n = SM.find g.preds_map n |> opt_to_lst
  
  let succs g n = succs_e g n |> List.map ~f:snd
  
  let preds g n = preds_e g n |> List.map ~f:snd

  let add_edge g ~src ~dst e =
    let old_succs = succs_e g src in
    let old_preds = preds_e g dst in
    let ms = SM.set ~key:src ~data:((e, dst)::old_succs) g.succs_map in
    let mp = SM.set ~key:dst ~data:((e, src)::old_preds) g.preds_map in
    { succs_map = ms; preds_map = mp }
  
end


type block_t = (string * Instr.t Array.t)

type t = { graph: G.t;(*The control flow graph*)
           args: Instr.dest list;
           order: string list;(*Blocks in original order*)
           ret_type: Bril_type.t option;
           func_name: string; (*Name of function this cfg represents*)
           map: block_t String.Map.t;(*yeah*) }


(** [next_block instrs info i] returns [(instrs1, info1)] where
 [info1] is [info] with fields updated to include the next block in
 [instrs], and [instrs1] are the remaining instructions. *)
let next_block (instrs: Instr.t list) (info: t) (i: int ref): Instr.t list * t =
  let open Instr in
  let name = match List.hd instrs with
    | Some (Label l) -> l
    | _ -> sprintf "_B%d" (i:=!i+1; !i) in
  print_endline name;
  (*The [curr] returned is in reversed order*)
  let rec step curr rest g =
    let open G in
    let src = name in begin
    match rest with
    | [] -> curr, rest, g
    (*Next three cases are terminators*)
    | Jmp l as h::t -> 
       h::curr, t, add_edge ~src ~dst:l g Jump
    | Br (_, l1, l2) as h::t ->
       let g1 = add_edge ~src ~dst:l1 g True in
       let g2 = add_edge ~src ~dst:l2 g1 False in
       h::curr, t, g2
    | Ret _ as h::t -> h::curr, t, g
    | h::((Label blk)::_ as rst)->
       h::curr, rst, add_edge ~src ~dst:blk g Next
    | h::t -> step (h::curr) t g end in
  let curr, rest, g = step [] instrs info.graph in
  let arr = Array.of_list_rev curr in
  let map1 = String.Map.add_exn ~key:name ~data:(name, arr) info.map in
  rest, { info with order=name::info.order; map = map1; graph = g }


(** Updates [info] recursively *)
let rec process_instrs instrs info i =
  match instrs with
  | [] -> info
  | _ -> let res = next_block instrs info i in
         process_instrs (fst res) (snd res) i
     

let of_func (funct: Func.t) =
  let init_info =
    { graph = G.empty;
      args = funct.args;
      order = [];
      ret_type = funct.ret_type;
      func_name = funct.name;
      map = String.Map.empty } in
  process_instrs funct.instructions init_info (ref 0) |>
    (fun inf -> { inf with order = List.rev inf.order; })


let to_func (g: t) : Func.t =
  let instrs =
    List.map
      ~f:(fun n -> SM.find_exn g.map n |> snd
                   |> Array.to_list)
      g.order
    |> List.concat in
  { name = g.func_name;
    args = g.args;
    ret_type = g.ret_type;
    instructions = instrs; }


let block_to_dot g b =
  let buf = Buffer.create 10 in
  Buffer.add_char buf '{';
  Buffer.add_string buf b;
  let arr = SM.find_exn g.map b |> snd in
  Array.iter arr ~f:(fun instr ->
      Buffer.add_char buf '|';
      Buffer.add_string buf (Instr.to_string instr);
    );
  Buffer.add_char buf '}';
  Buffer.contents buf


let to_dot ~names_only oc g =
  let open Out_channel in
  let prefix = g.func_name ^ "_69_420_" in
  output_string oc (sprintf "subgraph cluster_%s {\n" prefix);
  output_string oc (sprintf "label = \"%s\"\n" g.func_name);
  List.iter g.order ~f:(fun b ->
      let node =
        if names_only then (sprintf "%s%s [label=\"%s\"];\n" prefix b b)
        else (sprintf "%s%s [label=\"%s\" shape=\"record\"];\n" prefix b (block_to_dot g b)) in
      output_string oc node;);
  List.iter g.order ~f:(fun u ->
      u |> G.succs_e g.graph
      |> List.iter ~f:(fun p ->
             let edge_lbl = 
               match p with
               | (True, _) -> "[color=\"blue\"]"
               | (False, _) -> "[color=\"red\"]"
               | _ -> "" in
             output_string oc
               (sprintf "%s%s -> %s%s %s;\n" prefix u prefix (snd p) edge_lbl)));
  output_string oc "}"
  
  
(*
let to_dot ~uniq oc g =
 *)
