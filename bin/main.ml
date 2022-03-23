open! Core
open Yojson
open Global
open Ir

(**Local optimizations*)
let opt_local opt blck =
  ignore opt; blck

(**Global optimizations*)
let opt_global opt func =
  if String.(opt = "SSA")
  then Ssa.to_ssa func
  else if String.(opt = "LICM")
  then fst (Licm.insert_preheaders func)
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

let process_gen_cfg (dir, prog, fname): unit =
  let base = sprintf "%s_cfg.dot" fname in
  let path = Filename.concat dir base in
  let oc = Out_channel.create path in
  List.iter prog ~f:(fun (f:Func.t) ->
      Func.to_dot ~verbose:true ~oc f);
  Out_channel.close oc

let process_gen_dom (dir, prog, fname): unit =
  let base = sprintf "%s_dom.dot" fname in
  let path = Filename.concat dir base in
  let oc = Out_channel.create path in
  List.iter prog ~f:(fun (f:Func.t) ->
      (* let root = List.hd_exn f.order in *)
      let doms = Dominance.dominators f in
      (* let subtree = Cfg.Dominance.submissive_tree doms in
         let subfront = Cfg.Dominance.submissive_frontier doms f.graph in *)
      Dominance.to_dot ~oc ~label:f.name doms );
  Out_channel.close oc


let process_aux gen dir prog fname =
  let tuple = (dir, prog, fname) in
  let iterator g =
    if String.(g = "cfg")
    then process_gen_cfg tuple
    else if String.(g = "dom")
    then process_gen_dom tuple in
  List.iter gen ~f:iterator

let process ~opts ~gen ~out_dir ~out_path ~src_file =
  let ic, fname =
    match src_file with
    | None -> (In_channel.stdin, "tmp")
    | Some p ->
       (In_channel.create p,
       p |> Filename.basename |> Filename.chop_extension) in
  let oc =
    match out_path with
    | None -> Out_channel.stdout
    | Some p -> Out_channel.create p in
  let dir_aux = match out_dir with | None -> "." | Some p -> p in
  let prog : Bril.t = ic |> Basic.from_channel |> Bril.of_json in
  let prog' = List.fold opts
                   ~init: prog
                   ~f:(fun acc e -> optimize_single acc e) in
  process_aux gen dir_aux prog' fname;
  prog' |> Bril.to_json |> Basic.to_channel oc;
  In_channel.close ic;
  Out_channel.close oc
  

let command =
  Command.basic
    ~summary:"Brilliant, the compiler optimizer for BRIL"
    ~readme:(fun () -> "more info...")
    Command.Let_syntax.(
    let%map_open
        opts =
      flag "-O" (listed string)
        ~doc:
        "Optimizations to be executed sequentially.\n\
         Format: {l|g|i}[OPT], for local, global, and procedural
         optimizations/transformations."
    and gen =
      flag "-G" (listed string)
        ~doc:"What to generate, i.e. [cfg], [dom],..."
    and out_dir =
      flag "-d" (optional string)
        ~doc:"<path> Specify where to place the outputed files.\n\
              Default to current working directory"
    and out_path =
      flag "-D" (optional string)
        ~doc:"<path> Where to write the transformed program.\n\
              If not supplied, outputs to stdout"
    and src_file =
      flag "-S" (optional string)
        ~doc:"<path> location of the source file.\n\
              If not supplied, read from stdin instead." in
    fun () -> process ~opts ~gen ~out_dir ~out_path ~src_file)

let () = Command_unix.run ~version:"0.0.1" command
