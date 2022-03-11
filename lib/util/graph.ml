open! Core
open Sig

module MakeCommon(VI: VIngredient) = struct
  module VS = Set.Make(VI)
  module VM = Map.Make(VI)
  type v = VI.t
  
  let format =
    "fontname=\"Times\"\n\
     fontsize=\"20\"\n\
     penwidth=1\n\
     node[fontsize=\"16\" shape=\"box\" fontname=\"Times\"]\n"

  let default_np v =
    let lbl = VI.to_string v in
    sprintf "\"%s\" [label=\"%s\"]\n" lbl lbl

  let default_ep src dst =
    sprintf "\"%s\" -> \"%s\"\n"
      (VI.to_string src)
      (VI.to_string dst)
  
  let default_elp src _ dst =
    default_ep src dst
end


module MakeUnlabelled(VI: VIngredient) = struct

  include MakeCommon(VI)
  
  (*preds, succs*)
  type t = (VS.t * VS.t) VM.t
     
  let empty = VM.empty

  let rev g =
    VM.map g ~f:(fun (a, b) -> (b, a))
  
  let find g v =
    match VM.find g v with
    | None -> VS.empty, VS.empty
    | Some p -> p

  let full nodes : t =
    let all = VS.of_list nodes in
    List.fold nodes
      ~init: empty
      ~f:(fun g key ->
        VM.set g ~key ~data:(all, all))
  
  let preds g v : VS.t = find g v |> fst
  
  let succs g v : VS.t = find g v |> snd

  let add_vert g v =
    match VM.add g ~key:v ~data:(VS.empty, VS.empty) with
    | `Ok g' -> g'
    | `Duplicate -> g
    
  let add_edge g ~src ~dst =
    let preds_src, succs_src = find g src in
    let succs_src' = VS.add succs_src dst in
    let g = VM.set g ~key:src ~data:(preds_src, succs_src') in
    let preds_dst, succs_dst = find g dst in
    let preds_dst' = VS.add preds_dst src in
    VM.set g ~key:dst ~data:(preds_dst', succs_dst)

  let del_vert g v = VM.remove g v

  let del_edge g ~src ~dst =
    let preds_src, succs_src = find g src in
    let succs_src' = VS.remove succs_src dst in
    let g = VM.set g ~key:src ~data:(preds_src, succs_src') in
    let preds_dst, succs_dst = find g dst in
    let preds_dst' = VS.remove preds_dst src in
    VM.set g ~key:dst ~data:(preds_dst', succs_dst)

  let vert_lst g = VM.keys g

  let bfs g root =
    let set = ref VS.empty in
    let edges = Stack.create () in
    let queue = Queue.create () in
    Queue.enqueue queue root;
    set := VS.add !set root;
    while queue |> Queue.is_empty |> not do
      let u = Queue.dequeue_exn queue in
      VS.iter (succs g u)
        ~f:(fun v ->
          if not (VI.equal u v) && not (VS.mem !set v)
          then (Queue.enqueue queue v;
                set := VS.add !set v;
                Stack.push edges (u, v))
          else ())
    done;
    Stack.fold edges
      ~init:(add_vert empty root)
      ~f:(fun acc (u, v) -> add_edge acc ~src:u ~dst:v)

  let to_dot ~(oc:Out_channel.t) ~label
        ?(nf = default_np) ?(ef=default_ep) g =
    let open Out_channel in
    let print s = output_string oc (s^"\n") in
    print ("digraph {");
    print format;
    print "subgraph cluster_0 {";
    print (sprintf "label = \"%s\"" label);
    let nodes = vert_lst g in
    List.iter nodes ~f:(fun v -> v |> nf |> print);
    List.iter nodes ~f:(
        fun u -> succs g u
                 |> VS.iter ~f:(fun v -> ef u v |> print));
    print "}}"
end



module MakeLabelled(VI: VIngredient)(EI: EIngredient) = struct

  include MakeCommon(VI)

  module ES =
    Set.Make(
        struct
          type t = EI.t * VI.t [@@deriving sexp]
          let compare st1 st2 = VI.compare (snd st1) (snd st2)
        end)
  

  type e = EI.t
  type edge = e * v

  type t = (ES.t * ES.t) VM.t
     
  let empty = VM.empty
  
  let find g v = match VM.find g v with
    | None -> ES.empty, ES.empty
    | Some p -> p 

  let preds_e g v = v |> VM.find_exn g |> fst
  
  let succs_e g v = v |> VM.find_exn g |> snd

  let preds g v : VS.t =
    v |> preds_e g |> Set.to_array |> Array.unzip
    |> snd |> VS.of_sorted_array_unchecked
  
  let succs g v : VS.t =
    v |> succs_e g |> Set.to_array |> Array.unzip
    |> snd |> VS.of_sorted_array_unchecked

  let add_edge g ~src ~edg ~dst =
    let preds_src, succs_src = find g src in
    let succs_src' = ES.add succs_src (edg, dst) in
    let g = VM.set g ~key:src ~data:(preds_src, succs_src') in
    let preds_dst, succs_dst = find g dst in
    let preds_dst' = ES.add preds_dst (edg, src) in
    VM.set g ~key:dst ~data:(preds_dst', succs_dst)

  (*
  let del_edge g ~src ~dst =
    let old_succs = succs_e g src in
    let old_preds = preds_e g dst in
    let ms = VM.set ~key:src ~data:(ES.remove old_succs (edg, dst)) g.succs_map in
    let mp = VM.set ~key:dst ~data:(ES.remove old_preds (edg, src)) g.preds_map in
    { succs_map = ms; preds_map = mp } *)
  
  let add_vert g v =
    match VM.add g ~key:v ~data:(ES.empty, ES.empty) with
    | `Ok g' -> g'
    | `Duplicate -> g
  
  let del_vert g v = VM.remove g v

  let vert_lst g = VM.keys g

  let to_dot ~(oc:Out_channel.t) ~label
        ?(nf = default_np) ?(ef=default_elp) g =
    let open Out_channel in
    let print s = output_string oc (s^"\n") in
    print ("digraph {");
    print format;
    print "subgraph cluster_0 {";
    print (sprintf "label = \"%s\"" label);
    let nodes = vert_lst g in
    List.iter nodes ~f:(fun v -> v |> nf |> print);
    List.iter nodes ~f:(
        fun u -> succs_e g u
                 |> ES.iter ~f:(fun (e, v) -> ef u e v |> print));
    print "}}"
end
