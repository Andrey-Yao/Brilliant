open! Core
open Ir


module Value = struct
  module T = struct
    type t = {op: string; args: string list}
               [@@deriving equal, sexp]
    let compare (t1:t) (t2:t) = 0
  end
  include T
  include Comparator.Make(T)
end


module VM = Map.Make(Value)


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
type canon = Variable of string | Constant of Const.t


type data = {
    val_to_num : int VM.t;(*Row of values*)
    var_to_num : int String.Map.t;(*Cloud*)
    num_to_can : canon Int.Map.t;(*Canon*)
  }


let local_value_number_once data instr =
  let open Value in
  let open Instr in
  let op = Instr.opcode instr in
  let dest = Instr.dest instr in
  let args = Instr.args instr in
  match args, dest with
  | [], None -> data, instr
  | [], Some d -> failwith "TODO"
  | _ , None -> begin
      match VM.find data.val_to_num {op; args;} with
      | None -> failwith "TODO"
      | Some _ -> failwith "TODO" end
  | _ , Some d -> failwith "TODO"


let local_value_number args block =
  let init_info = {
    val_to_num = VM.empty; (*TODO change this bruh*)
    var_to_num = String.Map.empty;
    num_to_can = Int.Map.empty
  } in ()
