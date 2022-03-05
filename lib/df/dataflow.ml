open! Core
open Sig

module Forward (F : Frame) = struct
  type t = (F.p * F.p) String.Map.t

  (**[work_forward funct wdata wlist] works on the head of [wlist] and returns
     updated [wdata] and [wlist]. Does nothing if [wlist] is empty*)
  let work_forward (funct: Ir.Func.t) (wdata : t) (wlist : string list) =
    let getdata = String.Map.find_exn wdata in
    let open Ir.Func in
    match wlist with
    | [] -> (wdata, wlist)
    | b :: rest ->
        let preds = G.preds funct.graph b in
        let inb =
          G.VS.fold
            ~f:(fun a e -> getdata e |> snd |> F.meet a)
            ~init:F.top preds
        in
        let old_outb = getdata b |> snd in
        let new_outb = F.transfer inb (String.Map.find_exn funct.map b) in
        let wlist_new =
          if F.equal new_outb old_outb then rest
          else (G.succs funct.graph b |> G.VS.to_list) @ rest
        in
        let wdata_new = String.Map.set ~key:b ~data:(inb, new_outb) wdata in
        (wdata_new, wlist_new)

  let solve (funct: Ir.Func.t) : t =
    let initlist = funct.order in
    let initdata =
      List.fold
        ~f:(fun a e -> String.Map.set ~key:e ~data:(F.top, F.top) a)
        ~init:String.Map.empty funct.order
    in
    let rec helper (wdata, wlist) =
      match wlist with
      | [] -> wdata
      | _ -> helper (work_forward funct wdata wlist)
    in
    helper (initdata, initlist)
end


module Backward (F : Frame) = struct
  type t = (F.p * F.p) String.Map.t

  (**[work_backward funct wdata wlist] works on the head of [wlist] and returns
     updated [wdata] and [wlist]. Does nothing if [wlist] is empty*)
  let work_backward (funct: Ir.Func.t) (wdata : t) (wlist : string list) =
    let getdata = String.Map.find_exn wdata in
    let open Ir.Func in
    match wlist with
    | [] -> (wdata, wlist)
    | b :: rest ->
        let succs = G.succs funct.graph b in
        let outb =
          G.VS.fold
            ~f:(fun a e -> getdata e |> fst |> F.meet a)
            ~init:F.top succs
        in
        let old_inb = getdata b |> fst in
        let new_inb = F.transfer outb (String.Map.find_exn funct.map b) in
        let wlist_new =
          if F.equal old_inb new_inb then rest
          else (G.preds funct.graph b |> G.VS.to_list) @ rest
        in
        let wdata_new = String.Map.set ~key:b ~data:(outb, new_inb) wdata in
        (wdata_new, wlist_new)

  let solve (funct : Ir.Func.t) : t =
    let initlist = funct.order in
    let initdata =
      List.fold
        ~f:(fun a e -> String.Map.set ~key:e ~data:(F.top, F.top) a)
        ~init:String.Map.empty funct.order
    in
    let rec helper (wdata, wlist) =
      match wlist with
      | [] -> wdata
      | _ -> helper (work_backward funct wdata wlist)
    in
    helper (initdata, initlist)
end
