open! Core
open Util
open Common

type block_t = Instr.t List.t

type edge_lbl = True | False | Jump [@@deriving sexp]

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
    entry : string; (*entry block*)
    graph : G.t; (*The control flow graph*)
    ret_type : Bril_type.t option;
  }


(** [next_block instrs info i] returns [(instrs1, info1)] where
 [info1] is [info] with fields updated to include the next block in
 [instrs], and [instrs1] are the remaining instructions. Requires
 [instrs] to be nonempty. *)
let next_block (instrs : Instr.t list) (func : t) (i : int ref) :
    Instr.t list * t =
  let open Instr in
  let name, tail =
    match instrs with
    | Label l :: t -> l, t
    | _ ->
        sprintf "_B%d" (i := !i + 1; !i), instrs
  in
  let src = name in
  (*The [curr] returned is in reversed order*)
  let rec step curr rest g =
    let open G in
    match rest with
    | [] -> (curr, rest, g)
    (*Next three cases are terminators*)
    | (Jmp dst as h) :: t ->
       (h :: curr, t, add_edge ~src ~edg:Jump ~dst g)
    | (Br (_, l1, l2) as h) :: t ->
        let g1 = add_edge ~src ~edg:True ~dst:l1 g in
        let g2 = add_edge ~src ~edg:False ~dst:l2 g1 in
        (h :: curr, t, g2)
    | (Ret _ as h) :: t -> (h :: curr, t, add_vert g name)
    (*Peek ahead to see if it is a label next*)
    | (Label blk as h) :: rst ->
       let j = Jmp blk in
       (j :: curr, h :: rst, add_edge ~src ~edg:Jump ~dst:blk g)
    | h :: t -> step (h :: curr) t g
  in
  let graph = G.add_vert func.graph name in
  let curr, rest, graph' = step [] tail graph in
  let lst = List.rev curr in
  let map = String.Map.add_exn ~key:name ~data:lst func.map in
  (rest, { func with map; graph = graph' })

(** Updates [info] recursively *)
let rec process_instrs instrs func i =
  match instrs with
  | [] -> func
  | _ ->
      let res = next_block instrs func i in
      process_instrs (fst res) (snd res) i

let clean func =
  let reachable =
    let set = String.Hash_set.create () in
    let edges = Queue.create () in
    let queue = Queue.create () in
    Queue.enqueue queue func.entry;
    Hash_set.add set func.entry;
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
    G.VS.of_hash_set set in
  let unreachable =
    G.VS.diff (G.vert_lst func.graph |> G.VS.of_list) reachable in
  let graph, map =
    G.VS.fold unreachable
      ~init: (func.graph, func.map)
      ~f:(fun (g, m) v -> G.del_vert g v, SM.remove m v) in
  { func with graph; map }


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
  let instructions' =
    let instrs_rev = List.rev instructions in
    match instrs_rev with
    | Ret _ :: _ -> instructions
    | _ -> List.rev (Instr.Ret None :: instrs_rev) in
  let entry = match instructions with
    | Instr.Label lbl  :: _ -> lbl
    | _ -> "_B1" in
  let init_info = {
      args; name; ret_type; entry; graph = G.empty; map = String.Map.empty;
    } in
  process_instrs instructions' init_info (ref 0) |> clean


let to_json (func: t) : Yojson.Basic.t =
  let instrs =
    List.map (G.vert_lst func.graph)
      ~f:(fun n -> Instr.Label n :: (SM.find_exn func.map n))
    |> List.concat in 
  `Assoc
    ([
        ("name", `String func.name);
        ( "args",
          `List
            (List.map func.args ~f:(fun (name, bril_type) ->
                 `Assoc
                   [
                     ("name", `String name); ("type", Bril_type.to_json bril_type);
        ])) );
        ("instrs", `List (instrs |> List.map ~f:Instr.to_json));
      ]
     @ Option.value_map func.ret_type ~default:[] ~f:(fun t ->
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


let to_dot ~verbose ~oc f =
  let nf = (fun n -> sprintf "\"%s\" [label=%s shape=\"plaintext\"];\n"
                       n (block_to_dot f n))
  in
  let ef = (fun s e d -> begin match e with
             | True -> "[color=\"blue\"]"
             | False -> "[color=\"red\"]"
             | _ -> "" end |> sprintf "\"%s\" -> \"%s\" %s;\n" s d)
  in
  if not verbose
  then G.to_dot f.graph ~oc ~label:f.name ~ef
  else G.to_dot f.graph ~oc ~label:f.name ~nf ~ef


let map_blocks ~f func =
  let map_new = SM.map ~f func.map in
  { func with map = map_new }
