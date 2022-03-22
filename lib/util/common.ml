open! Core

let has_key json key =
  let open Yojson.Basic.Util in
  match json |> member key with `Null -> false | _ -> true

let to_list_nonnull =
  let open Yojson.Basic.Util in
  function `Null -> [] | json -> to_list json

module SM = String.Map
module SS = String.Set
module SHT = Hashtbl.Make(String)
module SHS = String.Hash_set

(**String * Int Set*)
module SIS =
  Set.Make(
      struct
        type t = string * int[@@deriving compare, hash, sexp]
      end)
