open! Core
open Yojson
open Ir
open Cfg

let to_dot_file ~name ~cfgs =
  let open Stdio__Out_channel in
  let fout = create (name ^ ".dot") in
  List.iter
    ~f:(fun g ->
      Cflow.to_dot ~names_only:false fout g;
      newline fout)
    cfgs;
  close fout

let process_single ~lvn ~outs ~srcpath ~outpath ~file =
  let in_prefix = match srcpath with | None -> "" | Some p -> p in
  let out_prefix = match outpath with | None -> "" | Some p -> p in
  print_endline out_prefix;
  let in_file = in_prefix ^ file in
  let yojson = Basic.from_file in_file in
  let name = String.chop_suffix_exn file ~suffix:".json" in
  let prog = Bril.of_json yojson in
  let cfgs = List.map prog ~f:Cflow.of_func in
  to_dot_file ~name:(out_prefix ^ name) ~cfgs;
  Basic.to_channel Out_channel.stdout yojson;
  Basic.to_file (out_prefix ^ name ^ "_opt.json") yojson


let process ~lvn ~outs ~srcpath ~outpath ~files =
  match files with
  | _::_ ->
     List.iter files ~f:(fun p ->
         process_single ~lvn ~outs ~srcpath ~outpath ~file:p)
  | [] ->
     let yojson = Basic.from_channel (In_channel.stdin) in
     let prog = Bril.of_json yojson in
     let cfgs = List.map prog ~f:Cflow.of_func in
     let yojson2 = List.map cfgs ~f:Cflow.to_func |> Bril.to_json in
     Basic.to_channel Out_channel.stdout yojson2

 
let command =
  Core.Command.basic ~summary:"Brilliant, the compiler optimizer for BRIL"
    ~readme:(fun () -> "more info...")
    Core.Command.Let_syntax.(
      let%map_open lvn =
        flag "-lvn" (listed string)
          ~doc:"Local value numbering related optimizations"
      and outs =
        flag "-out" (listed string)
          ~doc:
            "What to output. Default is just the json file.\n\
             Additional options: [cfg]"
      and outpath =
        flag "-D" (optional string)
          ~doc:"<path> Specify where to place the generated files"
      and srcpath =
        flag "-S" (optional string)
          ~doc:"<path> Specify where to find input source files"
      and files = anon (sequence ("filename" %: Core.Filename.arg_type)) in
      fun () -> process ~lvn ~outs ~srcpath ~outpath ~files)
