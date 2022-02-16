open! Core
open Ir


module Key = struct
  module T = struct
    type t = {op: string; args: string list}
               [@@deriving equal, sexp_of]
    let compare (t1:t) (t2:t) = 0
  end
  include T
  include Comparator.Make(T)
end


(* Annotates block with last write *)
let calc_last_write instrs : (Instr.t * bool) list =
  let empty = String.Set.empty in
  let folder set instr =
    match Instr.dest instr with
    | Some d ->
       let var = fst d in
       if String.Set.mem set var
       then (set, (instr, false))
       else (String.Set.add set var, (instr, true))
    | None -> (set, (instr, false)) in
  instrs |> List.rev |> List.folding_map ~init:empty ~f:folder |> List.rev


(** The type of canonical values. *)
type canon = string


type info = {
    val_to_num : int Map.M(Key).t;(*Row of values*)
    var_to_num : int String.Map.t;(*Cloud*)
    num_to_can : canon Int.Map.t;(*Canon*)
  }

  (*
let folding_mapper info (instr:Instr.t, last_write) =
  let open Key in
  let dest_opt = Instr.dest instr in
  let args = Instr.args instr in
  match args, dest_opt with
  | [], None -> (info, instr)(*Instructions with no args or dest*)
  | _, None -> begin 
     let key = {op=Instr.opcode instr; args} in
     match Map.find info.val_to_num key with
     | Some i -> failwith "hehe"
     | None -> 
    end
  | [], Some dest -> (info, instr)(*Can't do anything when no args*)
  | _, Some dest ->
     *)

     
let local_value_number (block: Instr.t list) = failwith "unimplemented"
