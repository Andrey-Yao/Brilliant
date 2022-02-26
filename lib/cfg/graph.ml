open! Core

module SM = String.Map

type 'a t = {
    succs_map : ('a * string) list SM.t;
    preds_map : ('a * string) list SM.t;
  }

let empty = { succs_map = SM.empty; preds_map = SM.empty }
let opt_to_lst = function None -> [] | Some s -> s
let succs_e g n = SM.find g.succs_map n |> opt_to_lst
let preds_e g n = SM.find g.preds_map n |> opt_to_lst
let succs g n = succs_e g n |> List.map ~f:snd
let preds g n = preds_e g n |> List.map ~f:snd

let add_edge g ~src ~dst e =
  let old_succs = succs_e g src in
  let old_preds = preds_e g dst in
  let ms = SM.set ~key:src ~data:((e, dst) :: old_succs) g.succs_map in
  let mp = SM.set ~key:dst ~data:((e, src) :: old_preds) g.preds_map in
  { succs_map = ms; preds_map = mp }

(**Removes [n] and its associated edges from [g]*)
let remove g n =
  let filter = List.filter ~f:(fun e -> e |> snd |> String.equal n) in
  let preds_map_1 =
    List.fold ~init:g.preds_map
      ~f:(fun acc s ->
        let hehe = SM.find acc n |> opt_to_lst |> filter in
        SM.set ~key:s ~data:hehe acc)
      (preds g n)
  in
  let succs_map_1 =
    List.fold ~init:g.succs_map
      ~f:(fun acc p ->
        let hehe = SM.find acc n |> opt_to_lst |> filter in
        SM.set ~key:p ~data:hehe acc)
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

let to_dot ~(oc:Out_channel.t) ~nodes ~label
      ?(nf = default_nf) ?(ef=defualt_ef) g =
  let open Out_channel in
  let print = output_string oc in
  print ("digraph {\n");
  print format;
  print "subgraph cluster_0 {\n";
  print (sprintf "label = \"%s\"\n" label);
  List.iter nodes ~f:(fun n -> n |> nf |> print; print "\n");
  List.iter nodes ~f:(fun src ->
      src |> succs_e g |> List.iter ~f:(fun (e, dst) ->
                              ef src e dst |> print;
                              print "\n"));
  print "}}\n"
