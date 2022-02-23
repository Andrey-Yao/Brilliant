open! Core
open Cfg
module SM = String.Map
module SS = String.Set

type set = Sett of SS.t | All
type t_abs = set SM.t
type t = set SM.t

(**Simulates the initialization of dominators to
   all the blocks*)
let extract = function None -> All | Some ss -> ss

let union s1 s2 =
  match (s1, s2) with
  | All, _ -> All
  | _, All -> All
  | Sett ss1, Sett ss2 -> Sett (SS.union ss1 ss2)

let inter s1 s2 =
  match (s1, s2) with
  | All, s -> s
  | s, All -> s
  | Sett ss1, Sett ss2 -> Sett (SS.inter ss1 ss2)

let equal s1 s2 =
  match (s1, s2) with
  | All, All -> true
  | Sett ss1, Sett ss2 -> SS.equal ss1 ss2
  | _ -> false

let reverse_post_order ~(order : string list) ~(graph : G.t) =
  (*Starts from [node] as root and does a postorder traversal
    while prepending elements to stack, while keeping track of
    which nodes have been visited via [set]*)
  let rec build node (set, stack) =
    if SS.mem set node then (set, stack)
    else
      let sett, stck =
        List.fold ~init:(set, stack)
          ~f:(fun (se, st) n -> build n (se, st))
          (G.succs graph node)
      in
      (SS.add sett node, node :: stck)
  in
  let rec trav_list (set, stack) rest =
    match rest with
    | [] -> (set, stack)
    | h :: _ ->
        let se, st = build h (set, stack) in
        trav_list (se, st) (List.filter ~f:(fun n -> SS.mem se n |> not) rest)
  in
  trav_list (SS.empty, []) order

(**Performs a single update in the dominator set for the 
   block [b]. Returns [None] if no change happened*)
let step_dom dom_map g b : t option =
  let doms_old = SM.find dom_map b |> extract in
  let ss =
    b |> G.preds g
    |> List.fold ~init:All ~f:(fun accum e ->
           e |> SM.find dom_map |> extract |> inter accum)
  in
  let doms_new = union ss (Sett (SS.singleton b)) in
  if equal doms_old doms_new then None
  else SM.set ~key:b ~data:doms_new dom_map |> Option.return

let find_dominators blocks (g : G.t) =
  ignore g;
  ignore blocks
