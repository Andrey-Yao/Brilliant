open! Core
open Yojson
open Ir
open Df

let to_dot_file ~name ~cfgs =
  let open Stdio__Out_channel in
  let fout = create (name ^ ".dot") in
  output_string fout
    ("digraph{\n\
      fontname=\"sans-serif\"\n\
      fontsize=\"24\"\n\
      penwidth=1\n\
      node[fontsize=\"20\" shape=\"box\" fontname=\"sans-serif\"]\n\
      label=\"" ^ name ^ "\"\n");
  List.iter
    ~f:(fun g ->
      Cfg.to_dot ~names_only:false fout g;
      newline fout)
    cfgs;
  output_string fout "}";
  close fout

let process ~lvn ~outs ~srcpath ~outpath ~file =
  let yojson = Basic.from_file file in
  let name = String.chop_suffix_exn file ~suffix:".json" in
  let prog = Bril.of_json yojson in
  let cfgs = List.map prog ~f:Cfg.of_func in
  to_dot_file ~name ~cfgs;
  Basic.to_file ("opt_" ^ file) yojson

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
      and file = anon ("filename" %: Core.Filename.arg_type) in
      fun () -> process ~lvn ~outs ~srcpath ~outpath ~file)
