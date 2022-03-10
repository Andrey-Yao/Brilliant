open! Core
open Dominance
open Util.Common
open Ir
module Dom = Dominance
module SHT = Hashtbl.Make(String)


(**Returns [func', n] where [func'] is [func] with all
   occurence of variables (including in the arguments)
   replaced with [v1,..., vn], and [n] is the number of
   such variables. This makes creating new variable
   names much easier later. *)
let preprocess (func: Func.t) =
  let func_args = List.map func.args ~f:fst in
  (*Map from var name to index*)
  let v2num = SHT.create () in
  (*Inits v2num to map arg0,...,argk to 0,...,k*)
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
  (*If func has arguments, add extra block at start*)
  let decoy_content = match func.args with
    | [] -> [ Instr.Nop ]
    | lst -> begin
        (*Copies argk into vk for each k*)
        List.mapi lst
          ~f:(fun i (a, t) ->
            let dst = (sprintf "v%d_0" i, t) in
            Instr.Unary (dst, Op.Unary.Id, a)) end in
  let decoy = "_decoy" in
  let new_map = SM.set (SM.map func.map ~f:process_blk)
                  ~key:decoy ~data:decoy_content in
  let new_graph = Func.G.add_edge func.graph
                    ~src:decoy ~edg:Func.Next
                    ~dst:(List.hd_exn func.order) in
  ({func with map = new_map;
              graph = new_graph;
              order = decoy :: func.order},
   SHT.length v2num)


(**[parse "v6"] is [6], for example*)
let parse var = String.slice var 1 0 |> int_of_string

(**Gives a map from variable names to their types and the
   blocks where they have definitions*)
let vars_to_defs_and_typs (func: Func.t) num_vars =
  let arr = Array.create ~len:num_vars
              (Bril_type.BoolType, SS.empty) in
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

(**Adding prepending phi function. Aware of labels*)
let stick_phi_in_funcs phi funcs =
  match funcs with
  | (Instr.Label _ as h) :: t -> h :: phi :: t
  | _ -> phi :: funcs 

(**First part of the algorithm... *)
let insert_phis (func: Func.t) domfrt var2typndefs =
  let num_vars = Array.length var2typndefs in
  (*has_phi_for_v[i] is the set of blocks with phi vi*)
  let has_phi_for_v = Array.create ~len:num_vars SS.empty in
  (*b2i is map from blocknames to instrs*)
  let folder_dom vi = fun b2i b ->
    let var = sprintf "v%d" vi in
    let instrs = SM.find_exn b2i b in
    let vtnd = var2typndefs.(vi) in
    let instrs_new =
      let hpfvi = has_phi_for_v.(vi) in
      (* v := phi l1 v ... ln v, where n is number of preds in cfg *)
      if SS.mem hpfvi b then instrs
      else let args = Func.G.preds func.graph b |> Func.G.VS.to_list
                      |> List.map ~f:(fun lbl -> (lbl, var)) in
           has_phi_for_v.(vi) <- SS.add hpfvi b;
           stick_phi_in_funcs (Instr.Phi ((var, fst vtnd), args)) instrs
    in
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
let esrap i stacks =
  match stacks.(i) with
  | [] -> "undefined" (*Undefined var*)
  | j :: _ -> sprintf "v%d_%d" i j


let for_loop_1_once instr counts stacks =
  let open Instr in
  match dest instr with
  | Some ((v, _) as d) -> 
     let instr' = match instr with
       | Phi _ -> instr
       | _ ->
          let uses =
            List.map (args instr)
              ~f:(fun v -> esrap (parse v) stacks) in
          set_args uses instr |> Option.value ~default:instr in
     let vi = parse v in
     let i = counts.(vi) in
     stacks.(vi) <- i :: stacks.(vi);
     counts.(vi) <- i + 1;
     set_dest d instr |> Option.value ~default:instr'
  | None -> instr


let for_loop_2_once y instr stacks =
  let open Instr in
  match instr with
  | Phi (dst, lst) ->
     let lst' = List.map lst
                  ~f:(fun (lbl, arg) ->
                    if String.(lbl = y)
                    then (lbl, esrap (parse arg) stacks)
                    else (lbl, arg)) in
     Phi (dst, lst')
  | _ -> instr


let rec rename_block_help dt b2i cfg block stacks counts =
  (*Old LHS to be popped at the end of functions*)
  
       


let rename_blks domtree (func: Func.t) =
  let num_args = List.length func.args in
  let stacks = Array.create ~len:num_args [] in
  let counts = Array.create ~len:num_args 0 in
  (*Because func args aliases are already defined in decoy*)
  for i = 0 to num_args - 1 do
    stacks.(i) <- 1 :: stacks.(i);
    counts.(i) <- 1
  done;
  (*Not decoy!! Actual entry block*)
  let entry_real = List.nth_exn func.order 1 in
  rename_block_help domtree func.map func.graph
    entry_real stacks counts
