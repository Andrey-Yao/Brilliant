open! Core
open Util.Common

module G =
  Util.Graph.MakeUnlabelled(
      struct
        type t = string [@@deriving compare, equal, sexp]
        let to_string s = s
      end)
module CFG = Ir.Func.G

type t = G.t

(**Gives the reverse postorder of [graph] starting from 
 [hd order] as a list. Unreachable nodes are omitted *)
let reverse_post_order ~order ~cfg
    : string list =
  (*Starts from [node] as root and does a postorder traversal
    while prepending elements to stack, while keeping track of
    which nodes have been visited via [set]*)
  let stack = Stack.create () in
  let set = String.Hash_set.create () in
  let rec traverse node: unit =
    if Hash_set.mem set node then ()
    else
      (Hash_set.add set node;
        CFG.VS.iter (CFG.succs cfg node)
            ~f:(fun v -> traverse v);
          Stack.push stack node) in
  match order with
  | [] -> []
  | entry :: _ ->
     traverse entry;
     Stack.to_list stack

(**Performs a single update in the dominator set for the 
   block [b] in [doms]. Returns [true] if change happened.
   What are you doing, step dom?*)
let step_dom (doms: G.VS.t SHT.t) (cfg: CFG.t) (b:string): bool =
  let open G in
  let find d v = SHT.find_exn d v in
  let doms_b_old = find doms b in
  let ss =
    let preds_b = CFG.preds cfg b in
    match CFG.VS.choose preds_b with
    | None -> VS.empty
    | Some pred ->
       CFG.VS.fold
         (CFG.preds cfg b)
         ~init:(find doms pred)
         ~f:(fun acc p -> acc |> VS.inter (find doms p))
  in
  let doms_b_new = VS.add ss b in
  if VS.equal doms_b_old doms_b_new then false
  else (SHT.set doms ~key:b ~data:doms_b_new; true)

(**[dominators f] gives a graph [g] such that the successors of 
   [v] in [g] are exactly the nodes that dominate [v].*)
let dominators (f: Ir.Func.t): t =
  let open G in
  let rpo = reverse_post_order ~order:f.order ~cfg:f.graph in
  let full = VS.of_list rpo in
  let doms = SHT.create () in
  List.iter rpo ~f:(fun b -> SHT.set doms ~key:b ~data: full);
  match rpo with
  | [] -> empty
  | h :: [] -> add_edge empty ~src:h ~dst:h
  | h :: t ->
     SHT.set doms ~key:h ~data:(VS.singleton h);
     let changed = ref true in
     (*Repeats till convergence*)
     while !changed do
       changed :=
         List.fold t ~init:false
           ~f:(fun acc b -> acc || (step_dom doms f.graph b))
     done;
     let folder b domz d = add_edge domz ~src:b ~dst:d in
     List.fold rpo
       ~init:empty
       ~f:(fun domz b ->
         VS.fold (SHT.find_exn doms b) ~init:domz ~f:(folder b))

(*TODO VERY INEFFICIENT RN*)
(**[idom doms u v] is when [u] immediately dominates [v]*)
let idom doms u v =
  let open G in
  let subs_u = preds doms u in
  let doms_v = succs doms v in
  let inter = VS.inter subs_u doms_v in
  VS.length inter = 2 && VS.equal inter (VS.add (VS.singleton u) v)

(**This gives a tree*)
let submissive_tree (doms: t) =
  let folder u g v =
    if idom doms u v then G.add_edge g ~src:u ~dst:v
    else G.add_vert (G.add_vert g u) v in
  List.fold (G.vert_lst doms)
    ~init:G.empty
    ~f:(fun g u ->
      G.VS.fold (G.preds doms u) ~init:g
        ~f:(folder u))

let submissive_frontier_single ~(df: t) ~(doms: t) ~(cfg: CFG.t) a =
  (*Nodes dominated by a*)
  let subs_a = G.preds doms a in
  let frontierish =
    G.VS.fold subs_a
      ~init: CFG.VS.empty
      ~f:(fun acc b ->
        b |> CFG.succs cfg |> CFG.VS.union acc)
    |> G.VS.map ~f:(fun x -> x) in
  G.VS.fold (G.VS.diff frontierish (G.VS.remove subs_a a))
    ~init:df
    ~f:(fun g b -> G.add_edge g ~src:a ~dst:b)

let submissive_frontier (doms: t) (cfg: CFG.t) =
  List.fold (CFG.vert_lst cfg)
    ~init:G.empty
    ~f:(fun g a ->
      let g' = G.add_vert g a in
      submissive_frontier_single ~df:g' ~doms ~cfg a)

let to_dot ~oc ~label doms =
  G.to_dot ~oc ~label doms
