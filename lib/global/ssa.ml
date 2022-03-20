open! Core
open Ir
open Util.Common
module Dom = Dominance

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
    ~f:(fun i v -> SHT.add_exn v2num ~key:v ~data:i);
  let map_var v =
    match SHT.find v2num v with
    | Some i -> sprintf "v%d" i
    | None -> let i = SHT.length v2num in
              SHT.add_exn v2num ~key:v ~data:i;
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
  let decoy_content = 
    (*Copies argk into vk for each k*)
    List.mapi func.args
      ~f:(fun i (a, t) ->
        let dst = (sprintf "v%d_0" i, t) in
        Instr.Unary (dst, Op.Unary.Id, a)) in
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
  let add_defs_and_typs b =
    List.iter (SM.find_exn func.map b)
      ~f:(fun instr ->
        match Instr.dest instr with
        | None -> ()
        | Some (v, typ) ->
           let i = parse v in
           arr.(i) <- (typ, SS.add (snd arr.(i)) b)) in
  func.order |> List.tl_exn |> List.iter  ~f:add_defs_and_typs;
  arr

(**Adding prepending phi function. Aware of labels*)
let stick_phi_in_func phi funcs =
  match funcs with
  | (Instr.Label _ as h) :: t -> h :: phi :: t
  | _ -> phi :: funcs 
  
(**First part of the algorithm... *)
let insert_phis (func: Func.t) subfront var2typndefs =
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
           (Instr.Phi ((var, fst vtnd), args)) :: instrs
    in
    let pair = var2typndefs.(vi) in
    var2typndefs.(vi) <- (fst pair, SS.add (snd pair) b);
    SM.set b2i ~key:b ~data:instrs_new in
  let folder_def vi = fun b2i d ->
    let subfront_d = Dom.G.succs subfront d in
    Dom.G.VS.fold subfront_d ~init:b2i ~f:(folder_dom vi) in
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
  let instr' = begin match instr with
  | Phi _ -> instr
  | _ ->
     let uses =
       List.map (args instr)
         ~f:(fun v -> esrap (parse v) stacks) in
     set_args uses instr |> Option.value ~default:instr end in
  match dest instr' with
  | Some (v, t) -> 
     let vi = parse v in
     let i = counts.(vi) in
     stacks.(vi) <- i :: stacks.(vi);
     counts.(vi) <- i + 1;
     set_dest (esrap vi stacks, t) instr' |> Option.value ~default:instr'
  | None -> instr'

let for_loop_2_once x stacks instr =
  let open Instr in
  match instr with
  | Phi (dst, lst) ->
     let lst' = List.map lst
                  ~f:(fun (lbl, arg) ->
                    if String.(lbl = x)
                    then (lbl, esrap (parse arg) stacks)
                    else (lbl, arg)) in
     Phi (dst, lst')
  | _ -> instr


let rec rename_block_help subtree b2i cfg stacks counts x =
  let instrs = SM.find_exn b2i x in
  (*Old LHS to be popped at the end of functions*)
  let old_asses = List.filter_map instrs ~f:Instr.dest in
  let instrs' =
    List.map instrs
      ~f:(fun ins -> for_loop_1_once ins counts stacks) in
  let b2i' = SM.set b2i ~key:x ~data:instrs' in
  let b2i'' =
    Func.G.VS.fold (Func.G.succs cfg x)
      ~init: b2i'
      ~f:(fun map y ->
        let ins = SM.find_exn map y in
        let ins' = List.map ins ~f:(for_loop_2_once x stacks) in
        SM.set map ~key:y ~data:ins') in
  let b2i''' = Dom.G.VS.fold (Dom.G.succs subtree x)
                 ~init: b2i''
                 ~f:(fun map y ->
                   rename_block_help subtree map cfg stacks counts y) in
  List.iter old_asses
    ~f:(fun dst ->
      let i = dst |> fst |> parse in
      stacks.(i) <- List.tl_exn stacks.(i));
  b2i'''


let rename_blks subtree num_vars (func: Func.t) : Func.t =
  let stacks = Array.create ~len:num_vars [] in
  let counts = Array.create ~len:num_vars 0 in
  (*Because func args aliases are already defined in decoy*)
  for i = 0 to (List.length func.args) - 1 do
    stacks.(i) <- 0 :: stacks.(i);
    counts.(i) <- 1
  done;
  (*Not decoy!! Actual entry block*)
  let entry_real =
    List.nth_exn func.order 1 in
  let b2i = rename_block_help subtree func.map func.graph
              stacks counts entry_real in
  { func with map = b2i }


let to_ssa (func: Func.t) =
  let func', num_vars = preprocess func in
  let var2typndefs = vars_to_defs_and_typs func' num_vars in
  let dominators = Dom.dominators func' in
  let subfront = Dom.submissive_frontier dominators func'.graph in
  let subtree = Dom.submissive_tree dominators in
  let func'' = insert_phis func' subfront var2typndefs in
  rename_blks subtree num_vars func''
  
