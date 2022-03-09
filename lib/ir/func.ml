open! Core
open Util
open Common

type block_t = Instr.t List.t

type edge_lbl = True | False | Jump | Next [@@deriving sexp]

module G =
  Graph.MakeLabelled(
      struct
        type t = string [@@deriving compare, equal, sexp]
        let to_string s = s
      end)(
      struct
        type t = edge_lbl [@@deriving sexp]
      end)


type t = {
    map : block_t SM.t; (*yeah*)
    args : Instr.dest list;
    name : string; (*Name of function*)
    graph : G.t; (*The control flow graph*)
    order : string list; (*Blocks in original or;der*)
    ret_type : Bril_type.t option;
  }


(** [next_block instrs info i] returns [(instrs1, info1)] where
 [info1] is [info] with fields updated to include the next block in
 [instrs], and [instrs1] are the remaining instructions. Requires
 [instrs] to be nonempty. *)
let next_block (instrs : Instr.t list) (info : t) (i : int ref) :
    Instr.t list * t =
  let open Instr in
  let name =
    match List.hd instrs with
    | Some (Label l) -> l
    | _ ->
        sprintf "_B%d"
          (i := !i + 1;
           !i)
  in
  (*The [curr] returned is in reversed order*)
  let rec step curr rest g =
    let open G in
    let src = name in
    match rest with
    | [] -> (curr, rest, g)
    (*Next three cases are terminators*)
    | (Jmp dst as h) :: t -> (h :: curr, t, add_edge ~src ~edg:Jump ~dst g)
    | (Br (_, l1, l2) as h) :: t ->
        let g1 = add_edge ~src ~edg:True ~dst:l1 g in
        let g2 = add_edge ~src ~edg:False ~dst:l2 g1 in
        (h :: curr, t, g2)
    | (Ret _ as h) :: t -> (h :: curr, t, g)
    | h :: ((Label blk :: _) as rst) ->
        (h :: curr, rst, add_edge ~src ~edg:Next ~dst:blk g)
    | h :: t -> step (h :: curr) t g
  in
  let curr, rest, g = step [] instrs info.graph in
  let lst = List.rev curr in
  let map1 = String.Map.add_exn ~key:name ~data:lst info.map in
  (rest, { info with order = name :: info.order; map = map1; graph = g })

(** Updates [info] recursively *)
let rec process_instrs instrs info i =
  match instrs with
  | [] -> info
  | _ ->
      let res = next_block instrs info i in
      process_instrs (fst res) (snd res) i


let clean func =
  let reachable =
    let root = List.hd_exn func.order in
    let set = String.Hash_set.create () in
    let edges = Queue.create () in
    let queue = Queue.create () in
    Queue.enqueue queue root;
    Hash_set.add set root;
    while queue |> Queue.is_empty |> not do
      let u = Queue.dequeue_exn queue in
      G.VS.iter (G.succs func.graph u)
        ~f:(fun v ->
          if String.(<>) u v && not (Hash_set.mem set v)
          then (Queue.enqueue queue v;
                Hash_set.add set v;
                Queue.enqueue edges (u, v))
          else ())
    done;
    SS.of_hash_set set in
  match func.order with
  | [] -> func
  | _ ->
     let unreachable = SS.diff (func.order |> SS.of_list) reachable in
     let order = List.filter ~f:(SS.mem reachable) func.order in
     let graph, map =
       SS.fold unreachable
         ~init: (func.graph, func.map)
         ~f:(fun (g, m) v -> G.del_vert g v, SM.remove m v) in
     { func with order; graph; map }


let of_json (json: Yojson.Basic.t) =
  let open Yojson.Basic.Util in
  let arg_of_json json =
    ( json |> member "name" |> to_string,
      json |> member "type" |> Bril_type.of_json )
  in
  let name = json |> member "name" |> to_string in
  let args =
    json |> member "args" |> to_list_nonnull |> List.map ~f:arg_of_json
  in
  let ret_type = json |> member "type" |> Bril_type.of_json_opt in
  let instructions =
    json |> member "instrs" |> to_list_nonnull |> List.map ~f:Instr.of_json
  in
  let init_info = {
      args; name; ret_type; order = []; graph = G.empty; map = String.Map.empty;
    }
  in
  process_instrs instructions init_info (ref 0)
  |> (fun inf -> { inf with order = List.rev inf.order }) |> clean


let to_json (g: t) : Yojson.Basic.t =
  let instrs =
    List.map ~f:(fun n -> SM.find_exn g.map n) g.order
    |> List.concat in 
  `Assoc
    ([
        ("name", `String g.name);
        ( "args",
          `List
            (List.map g.args ~f:(fun (name, bril_type) ->
                 `Assoc
                   [
                     ("name", `String name); ("type", Bril_type.to_json bril_type);
        ])) );
        ("instrs", `List (instrs |> List.map ~f:Instr.to_json));
      ]
     @ Option.value_map g.ret_type ~default:[] ~f:(fun t ->
           [ ("type", Bril_type.to_json t) ]))


let block_to_dot g b =
  let buf = Buffer.create 10 in
  let put s = Buffer.add_string buf s in
  put "<<table cellspacing=\"0\">\n";
  put (sprintf "<tr><td bgcolor=\"Green\">%s</td></tr>\n" b);
  List.iter (SM.find_exn g.map b)
    ~f:(fun instr ->
      instr |> Instr.to_string |> sprintf "<tr><td>%s</td></tr>\n" |> put);
  put"</table>>";
  Buffer.contents buf


let to_dot ~names_only oc f =
  let nf = (fun n -> sprintf "\"%s\" [label=%s shape=\"plaintext\"];\n"
                       n (block_to_dot f n))
  in
  let ef = (fun s e d -> begin match e with
             | True -> "[color=\"blue\"]"
             | False -> "[color=\"red\"]"
             | _ -> "" end |> sprintf "\"%s\" -> \"%s\" %s;\n" s d)
  in
  if names_only
  then G.to_dot f.graph ~oc ~nodes:f.order ~label:f.name ~ef
  else G.to_dot f.graph ~oc ~nodes:f.order ~label:f.name ~nf ~ef


let map_blocks ~f func =
  let map_new = SM.map ~f func.map in
  { func with map = map_new }
