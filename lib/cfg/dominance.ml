open! Core
open Util

module G =
  Graph.MakeUnlabelled(
      struct
        type t = string [@@deriving compare, equal, sexp]
        let to_string s = s
      end)

module CFG = Cflow.G
        
(**Simulates initializing to [all] in dom map*)
let extract all = function None -> all | Some s -> s

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
    else (let tmp = Cflow.G.VS.fold
            ~init:(SS.add set node)
            ~f:(fun se v -> build se v)
            (Cflow.G.succs cfg node) in
          Stack.push stack node; tmp) in
  match order with
  | [] -> []
  | entry :: _ ->
     let _: SS.t = build SS.empty entry in
     Stack.to_list stack


(**Performs a single update in the dominator set for the 
   block [b] from [doms]. Returns [None] if no change happened.
   What are you doing, step dom?*)
let step_dom (doms: G.t) (cfg: CFG.t) (b:string): G.t option =
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
         ~init:doms
         ~f:(fun acc d -> add_edge acc ~src:b ~dst:d)
       |> Option.return


(**Finds the dominators of each block given reverse postorder
   [rpo] and graph [g]. Omits unreachable blocks*)
let dominators (g: Cflow.t) =
  let open G in
  let rpo = reverse_post_order ~order:g.order ~cfg:g.graph in
  let folder (map, same) b =
    match step_dom map g.graph b with
    | None -> (map, same)
    | Some m -> (m, false)
  in
  (*Repeats till convergence*)
  let rec converge (map, same) =
    if (same) then map
    else converge (List.fold ~f:folder ~init:(map, true) rpo)
  in
  converge (full rpo, false)


(**Takes a dominator map [doms] to create a mapping from [blocks]
   to their immediate submissivers. *)
let submissive_tree blocks (doms: G.t) : G.t =
  let all = SS.of_list blocks in
  let find set ele = SM.find set ele |> extract all in
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


let dominance_frontier (subs: t) = ()


let format =
  "fontname=\"Times\"\n\
   fontsize=\"24\"\n\
   penwidth=1\n\
   node[fontsize=\"20\" shape=\"box\" fontname=\"Times\"]\n"


let to_dot oc (rel: t) blocks func_name =
  let open Out_channel in
  let print = output_string oc in
  let print_children n = begin
      match SM.find rel n with
      | None -> ()
      | Some set ->
         SS.iter set
         ~f:(fun e -> sprintf "\"%s\" -> \"%s\"\n" n e |> print)
    end in
  print ("digraph {\n");
  print format;
  print "subgraph cluster_0 {\n";
  print (sprintf "label = \"%s\"\n" func_name);
  List.iter blocks ~f:(fun b ->
      sprintf "\"%s\" [label=\"%s\"];\n" b b |> print;);
  List.iter blocks ~f:print_children;
  print "}}\n"
