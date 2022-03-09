open! Core
open Dominance
open Util.Common
open Ir
module Dom = Dominance
module SHT = Hashtbl.Make(String)


(**Returns [func', n] where [func'] is [func] with all
   occurence of variables (including in the arguments)
   replaced with [v1,..., vn], and [n] is the number of
   such arguments. This makes creating new variable
   names much easier later. *)
let global_variable_rename (func: Func.t) =
  let func_args = List.map func.args ~f:fst in
  (*Map from var name to index*)
  let v2num = SHT.create () in
  List.iteri func_args
    ~f:(fun i v -> SHT.set v2num ~key:v ~data:i);
  let map_var v =
    match SHT.find v2num v with
    | Some i -> sprintf "v%d" i
    | None -> let i = SHT.length v2num in
              SHT.set v2num ~key:v ~data:i;
              sprintf "v%d" i in
  let instr_folder instr =
    let args = Instr.args instr in
    let new_args = List.map args ~f:map_var in
    let instr1 = match Instr.set_args new_args instr with
      | Some inst -> inst
      | None -> instr in
    match Instr.dest instr1 with
    | Some (d, t) ->
       let d1 = map_var d in
       Option.value (Instr.set_dest (d1, t) instr1)
         ~default:instr1
    | None -> instr1 in
  let process_blk blk = List.map blk ~f:instr_folder in
  let new_map = SM.map func.map ~f:process_blk in
  let new_func_args =
    List.map func.args
      ~f:(fun (a, t) -> sprintf "v%d" (SHT.find_exn v2num a), t) in
  { func with map = new_map; args = new_func_args },
  SHT.length v2num

(**[parse "v6"] is [6], for example*)
let parse var = String.slice var 1 0 |> int_of_string

(**Gives a map from variable names to their types and the
   blocks where they have definitions*)
let vars_to_defs_and_typs (func: Func.t) n =
  let arr = Array.create ~len:n (Bril_type.BoolType, SS.empty) in
  let open Func.G in
  let add_defs_and_typs b =
    List.iter (SM.find_exn func.map b)
      ~f:(fun instr ->
        match Instr.dest instr with
        | None -> ()
        | Some (v, typ) ->
           let i = parse v in
           arr.(i) <- (typ, SS.add (snd arr.(i)) b)) in
  List.iter func.order ~f:add_defs_and_typs;
  arr

let insert_phi (func: Func.t) domfrt var2typndefs =
  (*Tail recursive and runtime is O(#phi-nodes) in instrs <3*)
  let rec has_phi_for_v instrs v =
    let open Instr in
    match instrs with
    | (Phi ((def, _), _)) :: _ when String.equal def v -> true
    | (Phi _) :: t -> has_phi_for_v t v
    | _ -> false in
  (*b2i is map from blocknames to instrs*)
  let folder_dom vi = fun b2i b ->
    let v = sprintf "v%d" vi in
    let instrs = SM.find_exn b2i b in
    let vtnd = var2typndefs.(vi) in
    let instrs_new =
      if has_phi_for_v instrs v then instrs
      else let arg = Dom.G.succs domfrt b |> Dom.G.VS.to_list
                     |> List.map ~f:(fun lbl -> (lbl, v)) in
           Instr.Phi ((v, fst vtnd), arg) :: instrs in
    let pair = var2typndefs.(vi) in
    var2typndefs.(vi) <- (fst pair, SS.add (snd pair) b);
    SM.set b2i ~key:b ~data:instrs_new in
  let folder_def vi = fun b2i d ->
    let domf_d = Dom.G.succs domfrt d in
    Dom.G.VS.fold domf_d ~init:b2i ~f:(folder_dom vi) in
  let folder_var = fun b2i vi ->
    var2typndefs.(vi) |> snd |> SS.fold ~init:b2i ~f:(folder_def vi) in
  let b2i_new =
    Array.foldi var2typndefs
      ~init:(func.map)
      ~f:(fun i b2i _ -> folder_var b2i i)
  in { func with map = b2i_new }

(**[esrap] is the opposite of [parse]*)
let esrap i j =
  if j = 0 then sprintf "v%d" i (*Undefined var*)
  else sprintf "v%d__%d" i j

let rec rename_block b2i domtree cfg block stack =
  let stack_backup = Array.copy stack in
  let for_ins_in_blk_once ins =
    let args =
      List.map (Instr.args ins)
        ~f:(fun v -> let i = parse v in
                     esrap i stack.(i)) in
    let ins1 = Option.value (Instr.set_args args ins)
                 ~default: ins in
    match Instr.dest ins1 with
    | Some (d, t) ->
       let i = parse d in
       stack.(i) <- stack.(i) + 1;
       let d1 = esrap i stack.(i) in
       Option.value (Instr.set_dest (d1, t) ins1)
         ~default: ins1
    | None -> ins1 in
  let for_blk_in_suc_once blk = fun suc ->
    let open Instr in
    let rec helper instrs = match instrs with
      | (Phi (d, lst)) :: t ->
         let lst_new =
           List.map lst
             ~f:(fun (lbl, var) ->
               if String.equal lbl blk then
                 let i = parse var in
                 (lbl, esrap i stack.(i))
               else (lbl, var)) in
         Phi (d, lst_new) :: helper t
      | lst -> lst in
    helper suc in
  let for1 =
    let data = List.map (SM.find_exn b2i block)
                 ~f:for_ins_in_blk_once in
    SM.set b2i ~key:block ~data in
  let for2 =


        (* for i = 0 to Array.length stack do
          stack.(i) <- stack_backup.(i) done *)
       
