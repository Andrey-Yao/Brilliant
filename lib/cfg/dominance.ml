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
let step_dom (doms: t) (func: CFG.t) (b:string): t option =
  let open G in
  let doms_b_old = succs doms b in
  let ss =
    CFG.VS.fold
      (CFG.preds func b)
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
let dominators (g: Ir.Func.t): t =
  let open G in
  let rpo = reverse_post_order ~order:g.order ~cfg:g.graph in
  let folder (doms, same) b =
    match step_dom doms g.graph b with
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
let spanning_tree root (doms: t) =
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


let dominance_frontier (doms: t) b = ()
