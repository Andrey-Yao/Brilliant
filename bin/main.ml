open! Core
open Yojson
open Ir


(**Local optimizations*)
let opt_local opt blck =
  ignore opt; blck

(**Global optimizations*)
let opt_global opt func =
  if String.(opt = "SSA")
  then Cfg.Ssa.to_ssa func
  else func

(**Interprocedural optimizations*)
let opt_universal opt prog =
  ignore opt; prog

let optimize_single prog optimization =
  let opt = String.slice optimization 1 0 in
  match String.get optimization 0 with
  | 'l' ->
     (*
     let map_block = List.map ~f:(opt_local opt) in
     List.map ~f:(opt_ opt) prog *)
     prog (*TODO uninplemented*)
  | 'g' ->
     List.map ~f:(opt_global opt) prog
  | 'i' -> opt_universal opt prog
  | _ -> failwith "Unsupported optimization type"

let process_genCfg ~genCfg prog : unit =
  let fname = match genCfg with
    | None -> "tmp_cfg.dot"
    | Some f -> f in
  let oc = Out_channel.create fname in
  Bril.to_dot prog ~verbose:false ~oc;
  Out_channel.close oc

let process_genDom ~genDom prog : unit =
  let fname = match genDom with
    | None -> "tmp_dom.dot"
    | Some f -> f in
  let oc = Out_channel.create fname in
  List.iter prog ~f:(fun (f:Func.t) ->
      let root = List.hd_exn f.order in
      let doms = Cfg.Dominance.dominators f in
      let domtree = Cfg.Dominance.dominance_tree root doms in
      let domfront = Cfg.Dominance.dominance_frontier doms f.graph in
      Cfg.Dominance.to_dot ~oc ~label:f.name domtree );
  Out_channel.close oc

let process ~opts ~srcpath ~outpath ~genCfg ~genDom =
  let ic =
    match srcpath with
    | None -> In_channel.stdin
    | Some p -> In_channel.create p in
  let oc =
    match outpath with
    | None -> Out_channel.stdout
    | Some p -> Out_channel.create p in
  let prog : Bril.t = ic |> Basic.from_channel |> Bril.of_json in
  let prog' = List.fold opts
                   ~init: prog
                   ~f:(fun acc e -> optimize_single acc e) in
  process_genCfg ~genCfg prog';
  process_genDom ~genDom prog';
  prog' |> Bril.to_json |> Basic.to_channel oc;
  In_channel.close ic;
  Out_channel.close oc
  

let command =
  Core.Command.basic
    ~summary:"Brilliant, the compiler optimizer for BRIL"
    ~readme:(fun () -> "more info...")
    Core.Command.Let_syntax.(
    let%map_open opts =
      flag "-opts" (listed string)
        ~doc:
        "What to output. Default is just the json file.\n\
         Additional options: [cfg]"
    and genCfg =
      flag "-cfg" (optional string)
        ~doc:"File name to output the cfg dot file"
    and genDom =
      flag "-dom" (optional string)
        ~doc:"File name to output the dominance graph"
    and outpath =
      flag "-D" (optional string)
        ~doc:"<path> Specify where to place the process program"
    and srcpath =
      flag "-S" (optional string)
        ~doc:"<path> Specify where to find input source file" in
    fun () -> process ~opts ~srcpath ~outpath ~genCfg ~genDom)

let () = Command.run ~version:"0.0.1" command
