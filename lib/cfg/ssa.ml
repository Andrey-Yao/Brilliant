open! Core
open Dominance
open Util.Common
open Ir
module Dom = Dominance

(**Gives a map from variable names to blocks where they have
   definitions and their types*)
let vars_to_defs_and_typs (func: Func.t) =
  let open Func.G in
  let add_defs_and_typs map b =
    List.fold (SM.find_exn func.map b)
      ~init:map
      ~f:(fun (v2t, v2d) instr ->
        match Instr.dest instr with
        | None -> (v2t, v2d)
        | Some (v, typ) ->
           let data = match SM.find v2d v with
             | None -> SS.singleton b
             | Some ss -> SS.add ss b in
           (SM.set v2t ~key:v ~data:typ,
            SM.set v2d ~key:v ~data)) in
  List.fold func.order
    ~init:(SM.empty, SM.empty)
    ~f:(fun map b -> add_defs_and_typs map b)

let insert_phi (func: Func.t) domfrt var2def var2typ =
  (*Tail recursive and runtime is O(#phi-nodes) in instrs <3*)
  let rec has_phi_for_v instrs v =
    match instrs with
    | (Instr.Phi ((def, _), _)) :: _ when String.equal def v -> true
    | (Instr.Phi _) :: t -> has_phi_for_v t v
    | _ -> false in
  (*b2i is blocks to instrs, v2d is vars to blocks where v is defined*)
  let folder_dom v = fun (b2i, v2d) b ->
    let instrs = SM.find_exn b2i b in
    let v_defs = SM.find_exn v2d v in
    let typ = SM.find_exn var2typ v in
    let instrs_new =
      if has_phi_for_v instrs v then instrs
      else let arg = Dom.G.succs domfrt b |> Dom.G.VS.to_list
                     |> List.map ~f:(fun lbl -> (lbl, v)) in
           Instr.Phi ((v, typ), arg) :: instrs in
    (SM.set b2i ~key:b ~data:instrs_new,
     SM.set v2d ~key:v ~data:(SS.add v_defs b)) in
  let folder_def v = fun acc d ->
    let domf_d = Dom.G.succs domfrt d in
    Dom.G.VS.fold domf_d ~init:acc ~f:(folder_dom v) in
  let folder_var = fun acc v ->
    let defs_v = SM.find_exn (snd acc) v in
    SS.fold defs_v ~init:acc ~f:(folder_def v) in
  let vars = SM.keys var2def in
  let b2i_new, _ = List.fold vars ~init:(func.map, var2def) ~f:folder_var
  in { func with map = b2i_new }


let for1
