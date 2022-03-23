open! Core
open Core_unix

let pipe_dot ~f ~ic ~oc =
  let fd_in = descr_of_in_channel ic in
  let fd_out = descr_of_out_channel oc in
  let fd_in1, fd_out1 = Core_unix.pipe () in
  match fork () with
  | `In_the_child ->
     dup2 ~src:fd_in ~dst:fd_in1 ();
     close fd_out1;
     close fd_in1;
     exec ~prog:"gvpack" ~argv:["-u"]
  | _ ->
     dup2 ~src:fd_out2 ~dst:fout ();
     close fd_out2;
     close fd_in2;
     exec ~prog:"dot" ~argv:["-Tpng"];
  let fd_in2, fd_out2 = Core_unix.pipe () in
  match fork () with
  | `In_the_child ->
     dup2 ~src:fd_in2 ~dst:fin ();
     close fd_out2;
     close fd_in2;
     exec ~prog:"gvpack" ~argv:["-u"]
  | _ ->
     dup2 ~src:fd_out2 ~dst:fout ();
     close fd_out2;
     close fd_in2;
     exec ~prog:"dot" ~argv:["-Tpng"]
