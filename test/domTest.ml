open! Core
open Util.Common
open OUnit2
open Printf
open Global

module CFG = Ir.Func.G
module Dom = Dominance
module G = Dom.G
type t = Dom.t

(**The dominance relation is a poset*)
module PosetProperties = struct

  (**Forall [b], [b] in [doms[b]]*)
  let reflexive_single (doms: t) b =
    G.VS.mem (G.succs doms b) b
  
  (**Forall [s] in [doms[b]], [doms[s] subseteq [doms[b]]]*)
  let transitive_single (doms: t) b =
    let doms_b = G.succs doms b in
    let predicate s =
      s |> G.succs doms |> G.VS.is_subset ~of_:doms_b in
    doms_b |> G.VS.for_all ~f:predicate

  (**Forall [b], [s] in [doms[b]] & [b] in [doms[s]]
     => [s] = [b]*)
  let antisymmetric_single (doms: t) b =
    let predicate s =
      (String.equal b s) || not (G.VS.mem (G.succs doms s) b) in
    b |> G.succs doms |> G.VS.exists ~f:predicate

  let make_test_single (doms: t) b = [
      sprintf "Reflexive [%s]" b >:: (fun _ ->
        assert_bool "ref" (reflexive_single doms b));
      sprintf "Transitive [%s]" b >:: (fun _ ->
        assert_bool "tra" (transitive_single doms b));
      sprintf "Antisymmetric [%s]" b >:: (fun _ ->
        assert_bool "ant" (antisymmetric_single doms b));
    ]

  let test_all doms blocks func_name =
    let tests =
      blocks |> List.map ~f:(make_test_single doms) |> List.concat
    in sprintf "Poset [%s] -> " func_name >::: tests
end


module DominanceProperties = struct

  let rec enumerate_paths_help g src dst seen path paths =
    Hash_set.add seen src;
    Queue.enqueue path src;
    if String.(src = dst)
    then paths := Queue.to_list path :: !paths
    else (CFG.VS.iter (CFG.succs g src)
           ~f:(fun suc ->
             if Hash_set.mem seen suc then ()
             else enumerate_paths_help g suc dst seen path paths));
    Hash_set.remove seen src

  let enumerate_paths g src dst =
    let seen = SHS.create () in
    let paths = ref [] in
    let path = Queue.create () in
    enumerate_paths_help g src dst seen path paths;
    List.map !paths ~f:(G.VS.of_list)

  let check_dom_single doms cfg entry v =
    let doms_v = G.succs doms v in
    let paths_v = enumerate_paths cfg entry v in (*
    List.iter paths_v ~f:(fun path ->
        path |> G.VS.to_list |> List.to_string ~f:String.to_string |> print_endline); *)
    List.for_all paths_v ~f:(fun p ->
        G.VS.is_subset doms_v ~of_:p)

  let test_all doms cfg name entry =
    let vertices = CFG.vert_lst cfg in
    let tests =
      List.map vertices
        ~f:(fun v ->
          sprintf "Doms of [%s]" v >::
            (fun _ ->
              assert_bool "path"
                (check_dom_single doms cfg entry v))) in
    sprintf "Dominance [%s]" name >::: tests
end


module DomFrontProperties = struct


end


module SubTreeProperties = struct

  let same_vertices doms1 doms2 =
    let s1 = G.vert_lst doms1 in
    let s2 = G.vert_lst doms2 in
    G.VS.equal (G.VS.of_list s1) (G.VS.of_list s2)

  let check_degree subtree =
    let verts = G.vert_lst subtree in
    List.for_all verts
      ~f:(fun v -> G.VS.length (G.preds subtree v) <= 1)

  (**Requires [subtree] to have only one connected component*)
  let idempotent subtree =
    let domtree = subtree |> Dom.G.rev |> Dom.submissive_tree in
    let verts = G.vert_lst domtree in
    List.for_all verts
      ~f:(fun v ->
        (G.VS.equal (G.succs domtree v) (G.succs domtree v))
        && (G.VS.equal (G.preds domtree v) (G.preds domtree v)))

  let test_all domtree doms name =
    sprintf "Domtree [%s]" name >:::
    [
      "Same vertices" >:: (fun _ ->
        assert_bool "ver" (same_vertices domtree doms));
      "Check_degree" >:: (fun _ ->
        assert_bool "deg" (check_degree domtree));
      "Idempotent">:: (fun _ -> 
        assert_bool "idm" (idempotent domtree));
    ]
end

let test_all (func: Ir.Func.t) =
  let entry = func.entry in
  let blks = func.graph |> CFG.vert_lst in
  let doms = Dom.dominators func in
  let domtree = Dom.submissive_tree doms in
  [
    PosetProperties.test_all doms blks func.name;
    DominanceProperties.test_all doms func.graph func.name entry;
    SubTreeProperties.test_all domtree doms func.name
  ]
