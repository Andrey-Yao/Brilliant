open! Core
open OUnit2
open Ir


let _ =
  Sys.chdir "test/benchmarks";
  let json_files = 
    Sys.readdir "."
    |> Array.to_list
    |> List.filter ~f:(fun x -> Filename.check_suffix x ".json") in
  List.iter json_files
    ~f:(fun file ->
      let ic = In_channel.create file in
      let prog = ic |> Yojson.Basic.from_channel |> Bril.of_json in
      let tests = List.map prog ~f:(fun f -> f |> DomTest.test_all) in
      Filename.chop_extension file >::: tests |> run_test_tt_main;
      In_channel.close ic)
