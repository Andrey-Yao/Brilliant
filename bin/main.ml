open! Core

let command =
  Core.Command.basic
    ~summary:
    "Compiler for BRIL language in CS6120"
    ~readme:(fun () ->
      "This is a compiler for the BRIL programming language developed by\n
       Andrey Yao for the class CS 6120 at Cornell University in SP2022.")
    let open Core.Command.Let_syntax in
      let%map_open lex =
        flag "--lex" no_arg ~doc:"Generate lexical analysis for the given files"
      and parse =
        flag "--parse" no_arg
          ~doc:"Generate syntactic analysis for the given files"
      and typecheck =
        flag "--typecheck" no_arg
          ~doc:"Generate type checking analysis for the given files"
      and irgen =
        flag "--irgen" no_arg
          ~doc:"Generate intermediate code for the given files"
      and irrun =
        flag "--irrun" no_arg
          ~doc:"Generate and interpret intermediate code for the given files"
      and astrun =
        flag "--astrun" no_arg
          ~doc:"Generate and interpret abstract syntax tree for the given files"
      and optir =
        flag "--optir" (listed string)
          ~doc:
            "<phase> Report the intermediate code at the specified phase \
             (initial or final) of optimization."
      and optcfg =
        flag "--optcfg" (listed string)
          ~doc:
            "<phase> Report the control-flow graph at the specified phase \
             (initial or final) of optimization."
