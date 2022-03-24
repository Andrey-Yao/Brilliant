open! Core
open Ir
open Util.Common
module CFG = Func.G
module DOM = Dominance.G
module SS = CFG.VS

(**Workflow is roughly:
   1. Calculate the headers (These should stay the same).
   2. For each header: Compute doms and then find all backedges
   from header, merge loops from it, and then insert preheader.
   3. Identify loop invariant instructions *)

(**[footers v cfg doms] is the set of footers of [v]*)
let footers_of_v v cfg doms =
  let subs_v = DOM.preds doms v in
  SS.filter (CFG.preds cfg v) ~f:(DOM.VS.mem subs_v)

(**Nodes that are headers*)
let compute_headers cfg doms =
  List.filter (CFG.vert_lst cfg)
    ~f:(fun v -> not (SS.is_empty (footers_of_v v cfg doms)))

(**https://pages.cs.wisc.edu/~fischer/cs701.f14/finding.loops.html
   [loop_of_backedge cfg header footer] is the natural loop around
   the edge header -> footer*)
let loop_of_backedge cfg header footer =
  let body = SHS.create () in
  let stack = Stack.singleton footer in
  Hash_set.add body header;
  while not (Stack.is_empty stack) do
    let d = Stack.pop_exn stack in
    if not (Hash_set.mem body d) then
      (Hash_set.add body d;
       CFG.VS.iter (CFG.preds cfg d)
         ~f:(Stack.push stack))
    else () done;
  SS.of_hash_set body


(**If multiple loops share the same header, combine as one*)
let loops_of_header header cfg doms =
  let footers_v = footers_of_v header cfg doms in
  List.map (SS.to_list footers_v)
    ~f:(fun footer -> loop_of_backedge cfg header footer)
  |> SS.union_list


(**[insert_preheader func loop head hair] inserts preheader
   infront of header... lol *)
let insert_preheader (func: Func.t) loops head hair: Func.t =
  (*edges to move*)
  let in_edges_hd =
    CFG.ES.filter (CFG.preds_e func.graph head)
      ~f:(fun v -> not (SS.mem loops (snd v))) in
  let new_cfg =
    CFG.ES.fold (in_edges_hd)
      ~init:(CFG.add_edge ~src:hair ~edg:Func.Jump
                ~dst:head func.graph)
      ~f:(fun acc (e, v) ->
        let g = CFG.del_edge acc v head in
        CFG.add_edge g ~src:v ~edg:e ~dst:hair) in
  let neu_map =
    SM.add_exn func.map
      ~key:hair
      ~data:[Instr.Jmp head] in
  let new_map =
    CFG.ES.fold (in_edges_hd)
      ~init:neu_map
      ~f:(fun acc (_, b) ->
        let instrs =
          List.map (SM.find_exn acc b)
            ~f:(fun ins -> Instr.update_labels ins head hair)
        in
        SM.set acc ~key:b ~data:instrs) in
  { func with map = new_map; graph = new_cfg }


let insert_preheaders (func: Func.t) : Func.t * string SM.t =
  let doms = Dominance.dominators func in
  let headers = compute_headers func.graph doms in
  let folder (func', map) h =
    let doms' = Dominance.dominators func' in
    let loops = loops_of_header h func'.graph doms' in
    let preh = "_pre_" ^ h ^ "_" in
    let func'' = insert_preheader func' loops h preh in
    let map' = SM.add_exn map ~key:h ~data:preh in
    (func'', map') in
  List.fold headers ~init:(func, SM.empty) ~f:folder


let find_loop_inv_instrs (func: Func.t) loop reaching_defs =
  (*Maps var name to definitions in loop*)
  let vars_2_defs = SHT.create () in
  let jam var blk index =
    let data = match Hashtbl.find vars_2_defs var with
      | None -> [(blk, index)]
      | Some lst -> (blk, index) :: lst in
    Hashtbl.set vars_2_defs ~key:var ~data in
  List.iter loop
    ~f:(fun b ->
      List.iteri (SM.find_exn func.map b)
        ~f:(fun i instr ->
          match Instr.dest instr with
          | Some (d, _) -> jam d b i
          | None -> ()));
  let changed = ref true in
  (* invars is a set of (block, index)*)
  let predicate_arg invars arg =
    match SHT.find_exn vars_2_defs arg with
    | [] -> true
    | h :: [] -> SIS.mem invars h
    | _ -> false in
  let foldi_instrs blk i invars instr =
    match Instr.dest instr with
    | Some (d, _) ->
       if SIS.mem invars (blk, i)
       then invars
       else if List.for_all (Instr.args instr)
               ~f:(predicate_arg invars)
       then (changed := true; SIS.add invars (blk, i))
       else invars
    | None -> invars in
  let iter_block invars b =
    List.foldi (SM.find_exn func.map b)
      ~init:invars
      ~f:(foldi_instrs b) in
  let invars = ref SIS.empty in
  while !changed do
    invars := List.fold loop ~init:!invars
                ~f:iter_block; done;
  !invars
