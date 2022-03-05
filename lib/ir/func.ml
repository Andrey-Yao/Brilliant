(*modified from https://github.com/sampsyo/bril/tree/main/bril-ocaml*)
open! Core
open! Common

type t = {
  name : string;
  args : Instr.dest list;
  ret_type : Bril_type.t option;
  instructions : Instr.t list;
}
[@@deriving compare, equal, sexp_of]

let of_json json =
  let open Yojson.Basic.Util in
  let arg_of_json json =
    ( json |> member "name" |> to_string,
      json |> member "type" |> Bril_type.of_json )
  in
  let name = json |> member "name" |> to_string in
  let args =
    json |> member "args" |> to_list_nonnull |> List.map ~f:arg_of_json
  in
  let ret_type = json |> member "type" |> Bril_type.of_json_opt in
  let instructions =
    json |> member "instrs" |> to_list_nonnull |> List.map ~f:Instr.of_json
  in
  { name; args; ret_type; instructions }

let to_json t =
  `Assoc
    ([
       ("name", `String t.name);
       ( "args",
         `List
           (List.map t.args ~f:(fun (name, bril_type) ->
                `Assoc
                  [
                    ("name", `String name); ("type", Bril_type.to_json bril_type);
                  ])) );
       ("instrs", `List (t.instructions |> List.map ~f:Instr.to_json));
     ]
    @ Option.value_map t.ret_type ~default:[] ~f:(fun t ->
          [ ("type", Bril_type.to_json t) ]))

let to_string { name; args; ret_type; instructions } =
  let header =
    sprintf "@%s%s%s {" name
      (match args with
      | [] -> ""
      | args ->
          sprintf "(%s)"
            (List.map args ~f:(fun (name, bril_type) ->
                 sprintf "%s: %s" name (Bril_type.to_string bril_type))
            |> String.concat ~sep:", "))
      (Option.value_map ret_type ~default:"" ~f:Bril_type.to_string)
  in
  let body =
    instructions
    |> List.map ~f:(fun instr ->
           sprintf
             (match instr with Label _ -> "%s:" | _ -> "  %s;")
             (Instr.to_string instr))
    |> String.concat ~sep:"\n"
  in
  sprintf "%s\n%s\n}" header body
