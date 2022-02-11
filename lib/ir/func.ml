(*modified from https://github.com/sampsyo/bril/tree/main/bril-ocaml*)
open! Core
open! Common

type t = {
  name : string;
  args : Instr.dest list;
  ret_type : Bril_type.t option;
  instructions: Instr.t list;
  }[@@deriving compare, equal, sexp_of]

let of_json json =
  let open Yojson.Basic.Util in
  let arg_of_json json =
    (json |> member "name" |> to_string, json |> member "type" |> Bril_type.of_json) in
  let name = json |> member "name" |> to_string in
  let args = json |> member "args" |> to_list_nonnull |> List.map ~f:arg_of_json in
  let ret_type = json |> member "type" |> Bril_type.of_json_opt in
  let instructions = json |> member "instrs" |> to_list_nonnull |> List.map ~f:Instr.of_json in
  { name; args; ret_type; instructions}

let to_json t =
  `Assoc
    ( [
        ("name", `String t.name);
        ( "args",
          `List
            (List.map t.args ~f:(fun (name, bril_type) ->
                 `Assoc [ ("name", `String name); ("type", Bril_type.to_json bril_type) ])) );
        ("instrs", `List (t.instructions |> List.map ~f:Instr.to_json));
      ]
    @ Option.value_map t.ret_type ~default:[] ~f:(fun t -> [ ("type", Bril_type.to_json t) ]) )

let to_string { name; args; ret_type; instructions } =
  let header =
    sprintf
      "@%s%s%s {"
      name
      ( match args with
      | [] -> ""
      | args ->
        sprintf
          "(%s)"
          ( List.map args ~f:(fun (name, bril_type) ->
                sprintf "%s: %s" name (Bril_type.to_string bril_type))
          |> String.concat ~sep:", " ) )
      (Option.value_map ret_type ~default:"" ~f:Bril_type.to_string)
  in
  let body = instructions |> List.map ~f:(fun instr ->
           sprintf
             ( match instr with
             | Label _ -> "%s:"
             | _ -> "  %s;" )
             (Instr.to_string instr))
    |> String.concat ~sep:"\n"
  in
  sprintf "%s\n%s\n}" header body

(*
module Dominance = struct
  module type S = sig
    type out

    val dominators : ?strict:bool -> t -> out String.Map.t
    val dominated : ?strict:bool -> t -> out String.Map.t
    val tree : t -> out String.Map.t * out String.Map.t
    val frontier : t -> out String.Map.t
    val back_edges : t -> out String.Map.t
  end

  module Sets : S with type out := String.Set.t = struct
    let preds_to_succs preds =
      Map.fold
        preds
        ~init:(Map.map preds ~f:(const String.Set.empty))
        ~f:(fun ~key:vertex ~data:vertices succs ->
          Set.fold vertices ~init:succs ~f:(fun succs pred ->
              Map.update succs pred ~f:(function
                  | None -> String.Set.singleton vertex
                  | Some vertices -> Set.add vertices vertex)))

    let dominators ?(strict = false) { order; preds; succs; _ } =
      let rec postorder (visited, vertices) vertex =
        if Set.mem visited vertex then (visited, vertices)
        else
          let (visited, vertices) =
            Map.find_exn succs vertex
            |> List.fold ~init:(Set.add visited vertex, vertices) ~f:postorder
          in
          (visited, vertices @ [ vertex ])
      in
      let (_, vertices) = postorder (String.Set.empty, []) (List.hd_exn order) in
      let rec compute dom =
        let new_dom =
          List.fold vertices ~init:dom ~f:(fun dom vertex ->
              if String.equal vertex (List.hd_exn order) then dom
              else
                let inter =
                  Map.find_exn preds vertex
                  |> List.map ~f:(Map.find_exn dom)
                  |> List.reduce ~f:Set.inter
                  |> Option.value ~default:String.Set.empty
                in
                Map.set dom ~key:vertex ~data:(Set.add inter vertex))
        in
        if String.Map.equal String.Set.equal dom new_dom then dom else compute new_dom
      in
      let init =
        List.mapi order ~f:(fun i vertex ->
            (vertex, if i = 0 then String.Set.singleton vertex else String.Set.of_list vertices))
        |> String.Map.of_alist_exn
      in
      compute init |> if strict then Map.mapi ~f:(fun ~key ~data -> Set.remove data key) else Fn.id

    let dominated ?strict = Fn.compose preds_to_succs (dominators ?strict)

    let tree func =
      let dominators = dominators ~strict:true func in
      let preds =
        Map.map dominators ~f:(fun doms ->
            Set.fold doms ~init:doms ~f:(fun doms dom ->
                Set.diff doms (Map.find_exn dominators dom)))
      in
      (preds, preds_to_succs preds)

    let frontier func =
      let dominated = dominated func in
      Map.map dominated ~f:(fun dominated ->
          Set.fold dominated ~init:String.Set.empty ~f:(fun frontier vertex ->
              Map.find_exn func.succs vertex
              |> List.filter ~f:(Fn.non (Set.mem dominated))
              |> String.Set.of_list
              |> Set.union frontier))

    let back_edges func =
      let dominators = dominators func in
      Map.mapi func.succs ~f:(fun ~key ~data ->
          List.filter data ~f:(fun succ -> Set.mem (Map.find_exn dominators key) succ)
          |> String.Set.of_list)
  end

  module Lists : S with type out := string list = struct
    let f = Map.map ~f:String.Set.to_list
    let dominators ?strict = Fn.compose f (Sets.dominators ?strict)
    let dominated ?strict = Fn.compose f (Sets.dominated ?strict)

    let tree func =
      let (preds, succs) = Sets.tree func in
      (f preds, f succs)

    let frontier = Fn.compose f Sets.frontier
    let back_edges = Fn.compose f Sets.back_edges
  end
end
 *)
