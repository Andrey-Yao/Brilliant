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
let step_dom (doms: t) (cfg: CFG.t) (b:string): t option =
  let open G in
  let doms_b_old = succs doms b in
  let ss =
    CFG.VS.fold
      (CFG.preds cfg b)
      ~init: VS.empty
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
  assert(VS.equal (VS.of_list rpo) (full rpo |> vert_lst |> VS.of_list));
  let folder (doms, same) b =
    match step_dom doms f.graph b with
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
  let set = String.Hash_set.create () in
  let edges = Queue.create () in
  let queue = Queue.create () in
  Queue.enqueue queue root;
  Hash_set.add set root;
  while queue |> Queue.is_empty |> not do
    let u = Queue.dequeue_exn queue in
    G.VS.iter (G.succs doms u)
      ~f:(fun v ->
        if String.(<>) u v && not (Hash_set.mem set v)
        then (Queue.enqueue queue v;
              Hash_set.add set v;
              Queue.enqueue edges (u, v))
        else ())
  done;
  Queue.fold edges
    ~init: G.empty
    ~f:(fun acc (u, v) -> G.add_edge acc ~src:u ~dst:v)


let dominance_frontier_single (doms: t) (cfg: CFG.t) b =
  (*Nodes one edge away from the submissive set*)
  let frontierish =
    G.VS.fold (G.succs doms b)
      ~init: CFG.VS.empty
      ~f:(fun acc s -> s |> CFG.succs cfg |> CFG.VS.union acc)
    |> CFG.VS.to_list |> G.VS.of_list in
  G.VS.diff frontierish (G.succs doms b)


let dominance_frontier (doms: t) (func: Ir.Func.t) =
  List.fold func.order
    ~init: G.empty
    ~f:(fun g1 src ->
      G.VS.fold (dominance_frontier_single doms func.graph src)
        ~init: g1
        ~f:(fun g2 dst -> G.add_edge g2 ~src ~dst))


let to_dot ~oc ~label doms =
  G.to_dot ~oc ~label doms
