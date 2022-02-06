open Yojson.Basic
open Ir

exception Malformed of string


let rec parse_type (json: t) =
  match json with
  | `String s -> begin
      match s with
      | "int" -> TInt
      | "bool" -> TBool
      | "float" -> TFloat
      | _ -> raise (Malformed "Unsupported type")
    end
  | `Assoc (h::[]) -> begin
      match fst h with
      | "ptr" -> TPtr (h |> snd |> parse_type)
      | _ -> raise (Malformed "Unsupport parametrized type")
    end
  | _ -> raise (Malformed "Expected type but got not type")
     
  
let parse_arg (json: t) =
  match json with
  | `Assoc lst ->
      let name = match List.assoc "name" lst with
        | `String s -> s
        | _ -> raise (Malformed "Expected variable name") in
      let tipe = lst |> List.assoc "type"|> parse_type in
     (name, tipe)
  | _ -> raise (Malformed "Expected name, type pair for arg")


let parse_instr (json: t) = ignore json; failwith "unimplemented"


let parse_func (json: t) : bril_func =
  match json with
  | `Assoc lst ->
     let name = match List.assoc "name" lst with
       | `String s -> s
       | _ -> raise (Malformed "Expected String for func name") in
     let args = match List.assoc "args" lst with
       | `List arg_lst -> List.map parse_arg arg_lst
       | _ -> raise (Malformed "Func args is not json array") in
     let tipe = lst |> List.assoc_opt "type" |> Option.map parse_type in
     let instrs = match List.assoc "instrs" lst with
       | `List instr_lst -> List.map parse_instr instr_lst
       | _ -> raise (Malformed "Expected array of instructions") in
     { name = name;
       args = args;
       instrs = instrs;
       ret_type = tipe;}
  | _ -> raise (Malformed "Expected function object")


let parse_file file_name = ignore file_name; failwith "unimplemented"
