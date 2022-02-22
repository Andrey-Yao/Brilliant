open! Core
open Yojson
open Ir
open Df

let process ~lvn ~outs ~srcpath ~outpath ~file =
  let yojson = Basic.from_file file in 
  let prog = Bril.of_json yojson in
  let hehe = prog |> List.map ~f:(fun a -> a |> (fun x ->
                                    let fu = Out_channel.create ("_" ^ a.name ^ ".dot") in
                                    let g = Cfg.of_func a in
                                    Cfg.to_dot_names_only fu g;
                                    Out_channel.close fu;
                                    g)|> Cfg.to_func) in
  let yojson2 = Bril.to_json hehe in
  Basic.to_file ("opt_"^file) yojson2


let command =
  Core.Command.basic
    ~summary:"Brilliant, the compiler optimizer for BRIL"
    ~readme:(fun () -> "more info...")
    Core.Command.Let_syntax.(
    let%map_open
          lvn = flag "--lvn" (listed string)
                      ~doc: "Local value numbering related optimizations"
      and outs = flag "--out" (listed string)
                   ~doc:"What to output. Default is just the json file.\n\
                         Additional options: [cfg]"
    and outpath = flag "-D" (optional string)
                       ~doc:"<path> Specify where to place the generated files"
    and srcpath = flag "-S" (optional string)
                       ~doc:"<path> Specify where to find input source files"
    and file = anon ("filename" %: Core.Filename.arg_type) in
    fun () -> process ~lvn ~outs ~srcpath ~outpath ~file)
