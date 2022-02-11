open! Core
open Ir


module Key = struct
  module T = struct
    type t =
      | BinExp of Op.Binary.t * int * int
      | UnaExp of Op.Unary.t * int
                                 [@@deriving equal, sexp_of]
    let compare (t1:t) (t2:t) = 0
  end
  include T
  include Comparator.Make(T)
end


module Map = Map.M(Key)

type value = {rownum: int; canon: string}

type map = value Map.t

(*
let folder (instr: Instr.t) (mesa, cloud) =
  let open Key in
  match instr with
  | Binary (dest, o, x, y) -> begin
      let i1 = 
      let key = T.BinExp (o, x, y) in
      
      
  |  

let local_value_number (block: Instr.t list) =
  
 *)
