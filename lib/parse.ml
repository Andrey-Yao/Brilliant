open Yojson.Basic
open! Base
open! Option.Monad_infix
open String
open Ir


let unaop_map : (string * unaop) list =
  [("load", Load); ("not", Not); ("id", Id)]

let binop_map : (string * binop) list =
  [("add", Add); ("mul", Mul); ("sub", Sub); ("div", Div); ("eq", Eq);
   ("lt", Lt); ("gt", Gt); ("le", Le); ("ge", Ge); ("and", And);
   ("or", Or); ("ptradd", PtrAdd)]

exception Malformed of string

type j = Yojson.t

let (let+) x f = Option.map ~f:f x
let (and+) = Option.both

(** Unwraps a target from a json string representation. *)
let unwrap_str : j -> string option =
  function `String s -> Some s | _ -> None
let unwrap_lst : j -> j list option =
  function `List l -> Some l | _ -> None
let unwrap_ass : j -> (string * j) list option =
  function `Assoc a -> Some a | _ -> None

let lst_assoc = List.Assoc.find ~equal:String.equal
let str_assoc v l = lst_assoc l v

let rec parse_type (json: j) : bril_type option =
  match json with
  | `String s -> begin
      match s with
      | "int" -> Some TInt
      | "bool" -> Some TBool
      | "float" -> Some TFloat
      | _ -> None end
  | `Assoc (h::[]) -> begin
      match fst h with
      | "ptr" -> (h |> snd |> parse_type) >>| (fun x -> TPtr x)
      | _ -> None end
  | _ -> None
     
  
let parse_arg json : (var * bril_type) option =
  let ass = json |> unwrap_ass in
  let+ name = ass >>= str_assoc "name" >>= unwrap_str
  and+ tipe = ass >>= str_assoc "type" >>= parse_type in
  (name, tipe)


let parse_inst json : bril_inst option =
  let get = function
    | Some a -> a
    | None -> raise (Malformed (Yojson.to_string json)) in
  json |> unwrap_ass >>= str_assoc "label"
  >>= unwrap_str >>| (fun x -> Label x) (*TODO not finished*)
         

let parse_func json : bril_func option =
  let ass = json |> unwrap_ass >>= str_assoc "functions"
            >>= unwrap_ass in
  let+ name = ass >>= str_assoc "name ">>= unwrap_str
  and+ args = ass >>= str_assoc "args" >>= unwrap_lst >>|
               List.map ~f:parse_arg >>= Option.all
  and+ instrs = ass >>= str_assoc "instructions" >>= unwrap_lst >>|
                  List.map ~f:parse_inst >>= Option.all in
  match ass >>= str_assoc "type" with
  | None ->
     { name = name;
       args = args;
       instrs = instrs;
       ret_type = None; }
  | Some json ->
     match parse_type json with
     | None -> raise (Malformed "No type defs found")
     | Some t -> { name = name;
       args = args;
       instrs = instrs;
       ret_type = Some t; }

let parse_file (file_name: string) =
  match from_file file_name with
  | `Assoc ((key, `List jsons)::[]) when key = "functions" ->
     List.map parse_func jsons
  | _ -> raise (Malformed "Expected a list of functions at top level of file")
