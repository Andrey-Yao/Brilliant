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
  let rec build set node: SS.t =
    if SS.mem set node then set
    else (let tmp = CFG.VS.fold
            ~init:(SS.add set node)
            ~f:(fun se v -> build se v)
            (CFG.succs cfg node) in
          Stack.push stack node; tmp) in
  match order with
  | [] -> []
  | entry :: _ ->
     let _: SS.t = build SS.empty entry in
     Stack.to_list stack

(**Performs a single update in the dominator set for the 
   block [b] from [doms]. Returns [None] if no change happened.
   What are you doing, step dom?*)
let step_dom all (doms:t) (cfg: CFG.t) (b:string): t option =
  let open G in
  let doms_b_old = succs doms b in
  let ss =
    CFG.VS.fold
      (CFG.preds cfg b)
      ~init:all
      ~f:(fun acc p -> acc |> VS.inter (succs doms p))
  in
  let doms_b_new = VS.add ss b in
  if VS.equal doms_b_old doms_b_new then None
  else VS.fold doms_b_new
         ~init:(G.del_vert doms b)
         ~f:(fun acc d -> add_edge acc ~src:b ~dst:d)
       |> Option.return

(**Finds the dominators of each block given reverse postorder
   [rpo] and graph [g]. Omits unreachable blocks*)
let dominators (f: Ir.Func.t): t =
  let open G in
  let rpo = reverse_post_order ~order:f.order ~cfg:f.graph in
  let all = VS.of_list rpo in
  let folder (doms, same) b =
    match step_dom all doms f.graph b with
    | None -> (doms, same)
    | Some m -> (m, false)
  in
  (*Repeats till convergence*)
  let rec converge (doms, same) =
    if (same) then doms
    else converge (List.fold ~f:folder ~init:(doms, true) rpo)
  in
  converge (full rpo, false)

(**This gives a tree*)
let dominance_tree root (doms: t) =
  G.bfs doms root

let dominance_frontier_single ~(df: t) ~(doms: t) ~(cfg: CFG.t) a =
  (*Nodes one edge away from the submissive set*)
  let doms_a = G.succs doms a in
  let frontierish =
    G.VS.fold doms_a
      ~init: CFG.VS.empty
      ~f:(fun acc b ->
        CFG.VS.remove (CFG.succs cfg b) b |> CFG.VS.union acc)
    |> CFG.VS.to_list |> G.VS.of_list in
  G.VS.fold (G.VS.diff frontierish doms_a)
    ~init:df
    ~f:(fun g b -> G.add_edge g ~src:a ~dst:b)


let dominance_frontier (doms: t) (cfg: CFG.t) =
  List.fold (CFG.vert_lst cfg)
    ~init:G.empty
    ~f:(fun g a ->
      dominance_frontier_single ~df:g ~doms ~cfg a)


let to_dot ~oc ~label doms =
  G.to_dot ~oc ~label doms
