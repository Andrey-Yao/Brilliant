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

  type t = {
      succs_map: VS.t VM.t;
      preds_map: VS.t VM.t
    }
     
  let empty = { succs_map = VM.empty; preds_map = VM.empty }
  
  let find m v =
    match VM.find m v with
    | None -> VS.empty
    | Some s -> s

  let full nodes : t =
    let data = VS.of_list nodes in
    List.fold nodes
      ~init: empty
      ~f:(fun acc key ->
        let succs_map = VM.set acc.succs_map ~key ~data in
        let preds_map = VM.set acc.preds_map ~key ~data in
        { succs_map; preds_map })
  
  let preds g v : VS.t = find g.preds_map v
  
  let succs g v : VS.t = find g.succs_map v

  let add_edge g ~src ~dst =
    let old_succs = succs g src in
    let old_preds = preds g dst in
    let ms = VM.set ~key:src ~data:(VS.add old_succs dst) g.succs_map in
    let mp = VM.set ~key:dst ~data:(VS.add old_preds src) g.preds_map in
    { succs_map = ms; preds_map = mp }

  (*
  let del_edge g ~src ~dst =
    let old_succs = succs_e g src in
    let old_preds = preds_e g dst in
    let ms = VM.set ~key:src ~data:(ES.remove old_succs (edg, dst)) g.succs_map in
    let mp = VM.set ~key:dst ~data:(ES.remove old_preds (edg, src)) g.preds_map in
    { succs_map = ms; preds_map = mp } *)

  (**Removes [n] and its associated edges from [g]*)
  let del_vert g v =
    let preds_map_1 =
      VS.fold
        ~init:g.preds_map
        ~f:(fun map s ->
          let old_preds = find map s in
          VM.set map ~key:s ~data:(VS.remove old_preds v))
        (succs g v)
    in
    let succs_map_1 =
      VS.fold
        ~init:g.succs_map
        ~f:(fun map p ->
          let old_succs = find map p in
          VM.set map ~key:p ~data:(VS.remove old_succs v))
        (preds g v)
    in
    { succs_map = VM.remove succs_map_1 v; preds_map = VM.remove preds_map_1 v; }


  let to_dot ~(oc:Out_channel.t) ~nodes ~label
        ?(nf = default_np) ?(ef=default_ep) g =
    let open Out_channel in
    let print s = output_string oc (s^"\n") in
    print ("digraph {");
    print format;
    print "subgraph cluster_0 {";
    print (sprintf "label = \"%s\"" label);
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

  type t = {
      succs_map: ES.t VM.t;
      preds_map: ES.t VM.t
    }
     
  let empty = { succs_map = VM.empty; preds_map = VM.empty }
  
  let find m v =
    match VM.find m v with
    | None -> ES.empty
    | Some s -> s

  let preds_e g v = find g.preds_map v
  let succs_e g v = find g.succs_map v

  let preds g v : VS.t =
    v |> preds_e g |> Set.to_array |> Array.unzip
    |> snd |> VS.of_sorted_array_unchecked
  
  let succs g v : VS.t =
    v |> succs_e g |> Set.to_array |> Array.unzip
    |> snd |> VS.of_sorted_array_unchecked

  let add_edge g ~src ~edg ~dst =
    let old_succs = succs_e g src in
    let old_preds = preds_e g dst in
    let ms = VM.set ~key:src ~data:(ES.add old_succs (edg, dst)) g.succs_map in
    let mp = VM.set ~key:dst ~data:(ES.add old_preds (edg, src)) g.preds_map in
    { succs_map = ms; preds_map = mp }

  (*
  let del_edge g ~src ~dst =
    let old_succs = succs_e g src in
    let old_preds = preds_e g dst in
    let ms = VM.set ~key:src ~data:(ES.remove old_succs (edg, dst)) g.succs_map in
    let mp = VM.set ~key:dst ~data:(ES.remove old_preds (edg, src)) g.preds_map in
    { succs_map = ms; preds_map = mp } *)

  (**Removes [n] and its associated edges from [g]*)
  let del_vert g v =
    let filter es = ES.filter ~f:(fun e -> e |> snd |> VI.equal v |> not) es in
    let preds_map_1 =
      VS.fold
        ~init:g.preds_map
        ~f:(fun map s ->
          let data = s |> find map |> filter in
          VM.set ~key:s ~data map)
        (succs g v)
    in
    let succs_map_1 =
      VS.fold
        ~init:g.succs_map
        ~f:(fun map p ->
          let data = p |> find map |> filter in
          VM.set ~key:p ~data map)
        (preds g v)
    in
    { succs_map = VM.remove succs_map_1 v; preds_map = VM.remove preds_map_1 v; }


  let to_dot ~(oc:Out_channel.t) ~nodes ~label
        ?(nf = default_np) ?(ef=default_elp) g =
    let open Out_channel in
    let print s = output_string oc (s^"\n") in
    print ("digraph {");
    print format;
    print "subgraph cluster_0 {";
    print (sprintf "label = \"%s\"" label);
    List.iter nodes ~f:(fun v -> v |> nf |> print);
    List.iter nodes ~f:(
        fun u -> succs_e g u
                 |> ES.iter ~f:(fun (e, v) -> ef u e v |> print));
    print "}}"
end
