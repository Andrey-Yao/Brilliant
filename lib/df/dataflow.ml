open! Core
open Cfg


module type Frame = sig
  
  type poset
  type t = (poset * poset) String.Map.t
  val top: poset
  val meet: poset -> poset -> poset
  val equal: poset -> poset -> bool
  val transfer: poset -> Ir.Instr.t list -> poset
  val optimize: Cfg.t -> t -> Cfg.t
end


module type DataFlow = sig
  type t
  val solve: Cfg.t -> t
  val optimize: Cfg.t -> t -> Cfg.t
end


module Forward(F: Frame) = struct

   type t = F.t
  
  (**[work_forward funct wdata wlist] works on the head of [wlist] and returns
     updated [wdata] and [wlist]. Does nothing if [wlist] is empty*)
  let work_forward (funct: Cfg.t) (wdata: t) (wlist: Cfg.Ver.t list) =
    let getdata = String.Map.find_exn wdata in 
    match wlist with
    | [] -> wdata, wlist
    | b::rest ->
       let preds = CFG.pred funct.graph b in
       let inb = List.fold ~f:(fun a e -> getdata e |> snd |> F.meet a)
                   ~init:F.top preds in
       let old_outb = getdata b |> snd in
       let new_outb = F.transfer inb (String.Map.find_exn funct.name_to_instrs b) in
       let wlist_new = if (F.equal new_outb old_outb) then rest
                       else (CFG.succ funct.graph b)@ rest in
       let wdata_new = String.Map.set ~key:b ~data:(inb, new_outb) wdata in
       wdata_new, wlist_new
    
  let solve (funct: Cfg.t) : t =
    let initlist = funct.blocks in
    let initdata = List.fold
                     ~f:(fun a e -> String.Map.set ~key:e ~data:(F.top, F.top) a)
                     ~init:String.Map.empty
                     funct.blocks in
    let rec helper (wdata, wlist) =
      match wlist with
      | [] -> wdata
      | _ -> helper(work_forward funct wdata wlist) in
    helper (initdata, initlist)

  let optimize = F.optimize
end


module Backward(F: Frame) = struct

  type t = F.t

  (**[work_backward funct wdata wlist] works on the head of [wlist] and returns
     updated [wdata] and [wlist]. Does nothing if [wlist] is empty*)
  let work_backward (funct: Cfg.t) (wdata: t) (wlist: Cfg.Ver.t list) =
    let getdata = String.Map.find_exn wdata in 
    match wlist with
    | [] -> wdata, wlist
    | b::rest ->
       let succs = CFG.succ funct.graph b in
       let outb = List.fold ~f:(fun a e -> getdata e |> fst |> F.meet a)
                    ~init:F.top succs in
       let old_inb = getdata b |> fst in
       let new_inb = F.transfer outb (String.Map.find_exn funct.name_to_instrs b) in
       let wlist_new = if (F.equal old_inb new_inb) then rest
                       else (CFG.succ funct.graph b)@ rest in
       let wdata_new = String.Map.set ~key:b ~data:(outb, new_inb) wdata in
       wdata_new, wlist_new
  
  let solve (funct: Cfg.t) : t =
    let initlist = funct.blocks in
    let initdata = List.fold
                     ~f:(fun a e -> String.Map.set ~key:e ~data:(F.top, F.top) a)
                     ~init:String.Map.empty
                     funct.blocks in
    let rec helper (wdata, wlist) =
      match wlist with
      | [] -> wdata
      | _ -> helper(work_backward funct wdata wlist) in
    helper (initdata, initlist)

  let optimize = F.optimize
end
