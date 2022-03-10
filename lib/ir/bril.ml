(*modified from https://github.com/sampsyo/bril/tree/main/bril-ocaml*)
open! Core
open Util.Common
module Bril_type = Bril_type
module Const = Const
module Func = Func
module Instr = Instr
module Op = Op

type t = Func.t list

let of_json json =
  let open Yojson.Basic.Util in
  json |> member "functions" |> to_list_nonnull |> List.map ~f:Func.of_json

let to_json prog =
  `Assoc [("functions", `List (List.map prog ~f:Func.to_json))]

let to_dot ~verbose ~oc prog =
  List.iter prog ~f:(Func.to_dot ~verbose ~oc)
