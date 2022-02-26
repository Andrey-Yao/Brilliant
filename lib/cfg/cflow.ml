open! Core
open Ir
open Util

type block_t = string * Instr.t Array.t

type edge_lbl = True | False | Jump | Next [@@deriving compare]

module G =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(
      struct
        type t = string [@@deriving compare, hash, equal]
      end)(
      struct
        type t = edge_lbl [@@deriving compare]
        let default = Next
      end)

type t = {
  graph : G.t; (*The control flow graph*)
  args : Instr.dest list;
  order : string list; (*Blocks in original order*)
  ret_type : Bril_type.t option;
  func_name : string; (*Name of function this cfg represents*)
  map : block_t SM.t; (*yeah*)
  }

let add_e ~src ~edg ~dst g =
  let e = G.E.create src edg dst in
  G.add_edge_e g e

let add_v ~ver g = G.add_vertex g ver

(** [next_block instrs info i] returns [(instrs1, info1)] where
 [info1] is [info] with fields updated to include the next block in
 [instrs], and [instrs1] are the remaining instructions. Requires
 [instrs] to be nonempty. *)
let next_block (instrs : Instr.t list) (info : t) (i : int ref) :
    Instr.t list * t =
  let open Instr in
  let name =
    match List.hd instrs with
    | Some (Label l) -> l
    | _ ->
        sprintf "_B%d"
          (i := !i + 1;
           !i)
  in
  (*The [curr] returned is in reversed order*)
  let rec step curr rest g =
    let open G in
    let src = name in
    match rest with
    | [] -> (curr, rest, g)
    (*Next three cases are terminators*)
    | (Jmp dst as h) :: t -> (h :: curr, t, add_e ~src ~edg:Jump ~dst g)
    | (Br (_, l1, l2) as h) :: t ->
        let g1 = add_e ~src ~edg:True ~dst:l1 g in
        let g2 = add_e ~src ~edg:True ~dst:l2 g1 in
        (h :: curr, t, g2)
    | (Ret _ as h) :: t -> (h :: curr, t, g)
    | h :: (Label blk :: _ as rst) ->
        (h :: curr, rst, add_e ~src ~edg:Next ~dst:blk g)
    | h :: t -> step (h :: curr) t g
  in
  let curr, rest, g = step [] instrs info.graph in
  let arr = Array.of_list_rev curr in
  let map1 = String.Map.add_exn ~key:name ~data:(name, arr) info.map in
  (rest, { info with order = name :: info.order; map = map1; graph = g })

(** Updates [info] recursively *)
let rec process_instrs instrs info i =
  match instrs with
  | [] -> info
  | _ ->
      let res = next_block instrs info i in
      process_instrs (fst res) (snd res) i

let of_func (funct: Func.t) =
  let init_info =
    {
      graph = G.empty;
      args = funct.args;
      order = [];
      ret_type = funct.ret_type;
      func_name = funct.name;
      map = String.Map.empty;
    }
  in
  process_instrs funct.instructions init_info (ref 0) |> fun inf ->
  { inf with order = List.rev inf.order }

let to_func (g: t) : Func.t =
  let instrs =
    List.map ~f:(fun n -> SM.find_exn g.map n |> snd |> Array.to_list) g.order
    |> List.concat
  in
  {
    name = g.func_name;
    args = g.args;
    ret_type = g.ret_type;
    instructions = instrs;
  }


let block_to_dot g b =
  let buf = Buffer.create 10 in
  Buffer.add_char buf '{';
  Buffer.add_string buf b;
  let arr = SM.find_exn g.map b |> snd in
  Array.iter arr ~f:(fun instr ->
      Buffer.add_char buf '|';
      Buffer.add_string buf (Instr.to_string instr));
  Buffer.add_char buf '}';
  Buffer.contents buf


let to_dot ~names_only oc (g: t) =
  let open Graph.Graphviz in
  let module D = Graph.Graphviz.Dot(
    struct
      include G
      let graph_attributes = Util.g_attrs ~label:g.func_name
      let vertex_name = Util.vertex_name
      let vertex_attributes v =
        let label =
          if names_only then v
          else sprintf "%s [label=\"%s\" shape=\"record\"];\n" v
                 (block_to_dot g v) in
        (Util.v_attrs label)
      let edge_attributes e = match G.E.label e with
        | True -> [ `Color 0xff ]
        | False -> [ `Color 0xff0000 ]
        | _ -> []
      let default_vertex_attributes = Util.default_v_attrs
      let default_edge_attributes = Util.default_e_attrs
      let get_subgraph = Util.get_subgraph
    end) in
  D.output_graph oc g.graph
  

(**Cleans [graph] so every node is reachable from entry*)
let remove_unreachable graph =
  let rec traverse s n =
    if SS.mem s n then s
    else n |> G.succ graph.graph |> List.map ~f:(traverse s) |> SS.union_list
  in
  match graph.order with
  | [] -> graph
  | start :: _ ->
      let reachable = traverse SS.empty start in
      let folder (g, ord) n =
        if SS.mem reachable n then (g, n :: ord)
        else
          ({ g with graph = G.remove_vertex g.graph n; map = SM.remove g.map n }, ord)
      in
      let g, ord = folder (graph, []) start in
      { g with order = List.rev ord }
