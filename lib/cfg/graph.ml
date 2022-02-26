open Util
open Printf

module Make(Edg: Sig.E) = struct
  
  module ES = Set.Make(struct
                  type t = Edg.t * string
                  let compare e1 e2 = String.compare (snd e1) (snd e2) end)
  type e = Edg.t
  type t = {
      succs_map : ES.t SM.t;
      preds_map : ES.t SM.t;
    }

  let empty = { succs_map = SM.empty; preds_map = SM.empty }
  let opt_to_set = function None -> ES.empty | Some s -> s
  let succs_e g n = SM.find g.succs_map n |> opt_to_set
  let preds_e g n = SM.find g.preds_map n |> opt_to_set
  let succs g n : SS.t =
    succs_e g n |> ES.to_seq |> Seq.map snd |> List.of_seq |> SS.of_list
  let preds g n : SS.t =
    preds_e g n |> ES.to_seq |> Seq.map snd |> List.of_seq |> SS.of_list

  let add_edge g ~src ~dst e =
    let old_succs = succs_e g src in
    let old_preds = preds_e g dst in
    let ms = SM.set ~key:src ~data:(ES.add (e, dst) old_succs) g.succs_map in
    let mp = SM.set ~key:dst ~data:(ES.add (e, src) old_preds) g.preds_map in
    { succs_map = ms; preds_map = mp }

  (**Removes [n] and its associated edges from [g]*)
  let remove g n =
    let filter = ES.filter (fun e -> e |> snd |> String.equal n) in
    let preds_map_1 =
      SS.fold
        ~f:(fun acc s ->
          let preds_s = SM.find acc n |> opt_to_set |> filter in
          SM.add_exn ~key:s ~data:preds_s acc)
        ~init:g.preds_map
        (preds g n)
    in
    let succs_map_1 =
      SS.fold
        ~f:(fun acc p ->
          let succs_s = SM.find acc n |> opt_to_set |> filter in
          SM.add_exn ~key:p ~data:succs_s acc)
        ~init:g.succs_map
        (succs g n)
    in
    { succs_map = SM.remove succs_map_1 n; preds_map = SM.remove preds_map_1 n; }

  let format =
    "fontname=\"Times\"\n\
     fontsize=\"20\"\n\
     penwidth=1\n\
     node[fontsize=\"16\" shape=\"box\" fontname=\"Times\"]\n"

  let default_nf = fun n -> sprintf "\"%s\" [label=\"%s\"]\n" n n

  let defualt_ef = fun src _ dst -> sprintf "\"%s\" -> \"%s\"\n" src dst

  let to_dot ~(oc:Stdio.Out_channel.t) ~nodes ~label
        ?(nf = default_nf) ?(ef=defualt_ef) g =
    let open Stdio.Out_channel in
    let print = output_string oc in
    print ("digraph {\n");
    print format;
    print "subgraph cluster_0 {\n";
    print (sprintf "label = \"%s\"\n" label);
    let set_iter src =
      fun (e, dst) -> ef src e dst |> print; print "\n" in
    List.iter (fun n -> n |> nf |> print; print "\n") nodes;
    List.iter (fun src -> src |> succs_e g |> ES.iter (set_iter src)) nodes;
    print "}}\n"

end
