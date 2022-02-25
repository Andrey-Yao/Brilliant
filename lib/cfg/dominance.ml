open! Core
module SM = String.Map
module SS = String.Set

type t = SS.t SM.t

(**Simulates initializing to [all] in dom map*)
let extract all = function None -> all | Some s -> s

(**Gives the reverse postorder of [graph] starting from 
 [hd order] as a list. Unreachable nodes are omitted *)
let reverse_post_order ~(order : string list) ~(graph : Graph.t) =
  (*Starts from [node] as root and does a postorder traversal
    while prepending elements to stack, while keeping track of
    which nodes have been visited via [set]*)
  let rec build node (set, stack) =
    if SS.mem set node then (set, stack)
    else
      let sett, stck =
        List.fold ~init:(set, stack)
          ~f:(fun (se, st) n -> build n (se, st))
          (Graph.succs graph node)
      in
      (SS.add sett node, node :: stck)
  in
  match order with
  | [] -> []
  | start :: _ -> build start (SS.empty, []) |> snd
  (*let rec trav_list (set, stack) rest =
    match rest with
    | [] -> (set, stack)
    | h :: _ ->
        let se, st = build h (set, stack) in
        trav_list (se, st) (List.filter ~f:(fun n -> SS.mem se n |> not) rest)
  in
  trav_list (SS.empty, []) order *)

(**Performs a single update in the dominator set for the 
   block [b] from [doms]. Returns [None] if no change happened.
   What are you doing, step dom?*)
let step_dom (doms: t) all g b: t option =
  let doms_b_old = b |> SM.find doms |> extract all in
  let ss =
    b |> Graph.preds g
    |> List.fold ~init:all ~f:(fun accum e ->
           e |> SM.find doms |> extract all |> SS.inter accum)
  in
  let doms_b_new = SS.union ss (SS.singleton b) in
  if SS.equal doms_b_old doms_b_new then None
  else SM.set ~key:b ~data:doms_b_new doms |> Option.return


(**Finds the dominators of each block given reverse postorder
   [rpo] and graph [g]. Omits unreachable blocks*)
let dominators rpo (g : Graph.t) =
  let all = SS.of_list rpo in
  let folder (map, same) b =
    match step_dom map all g b with
    | None -> (map, same)
    | Some m -> (m, false)
  in
  (*Repeats till convergence*)
  let rec converge (map, same) =
    if (same) then (map, true)
    else converge (List.fold ~f:folder ~init:(map, true) rpo)
  in
  converge (SM.empty, false) |> fst


(**[invert blocks doms] gives the submissive map of blocks.
   This function should be an involution*)
let invert blocks (doms: t) : t =
  let all = SS.of_list blocks in
  let add_to_set map key aad =
    match SM.find map key with
    | None -> SM.add_exn ~key ~data:(SS.singleton aad) map
    | Some set -> SM.set ~key ~data:(SS.add set aad) map in
  let lst_folder subs b =
    SS.fold
      ~init:subs
      ~f:(fun acc e -> add_to_set acc e b)
      (SM.find doms b |> extract all) in
  List.fold ~init:SM.empty ~f:lst_folder blocks


(**Takes a dominator map [doms] to create a mapping from [blocks]
   to their immediate submissivers. *)
let submissive_tree blocks (doms: t) : t =
  let all = SS.of_list blocks in
  let find set ele = SM.find set ele |> extract all in
  let subs = invert blocks doms in
  (*s is an immediate submissive of b iff doms[s] intersect
    subs[b] is exactly {s, b} and s =\= b*)
  let isubs_b b =
    let subs_b = find subs b in
    SS.filter
      ~f:(fun s -> SS.inter subs_b (find doms s)
                   |> SS.equal (SS.of_list [s; b])
                   |> (&&) (String.(<>) b s))
      subs_b in
  List.fold
    ~init:SM.empty
    ~f:(fun acc e -> SM.add_exn ~key:e ~data:(isubs_b e) acc)
    blocks


(*
let to_dot oc map blocks func_name =
  let open Out_channel in
  let print = output_string oc in
  let print_children n =
    match SM.find map n with
    | None -> ()
    | Some (Sset d)-> 
  print ("digraph {\n");
  print "subgraph cluster_0 {";
  print (sprintf "label = \"%s\"\n" func_name);
  List.iter blocks ~f:(fun b ->
      sprintf "%s [label=\"%s\"];\n" b b |> print;);
  
 *)
