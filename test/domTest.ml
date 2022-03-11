open! Core
open OUnit2
open Printf
open Cfg

module Dom = Dominance
module G = Dom.G
type t = Dom.t

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


end


module DomTreeProperties = struct

  let same_vertices doms1 doms2 =
    let s1 = G.vert_lst doms1 in
    let s2 = G.vert_lst doms2 in
    G.VS.equal (G.VS.of_list s1) (G.VS.of_list s2)

  let check_degree domtree =
    let verts = G.vert_lst domtree in
    List.for_all verts
      ~f:(fun v -> G.VS.length (G.preds domtree v) <= 1)

  let idempotent domtree =
    let domtree' = Dom.submissive_tree domtree in
    let verts = G.vert_lst domtree in
    List.for_all verts
      ~f:(fun v ->
        (G.VS.equal (G.succs domtree v) (G.succs domtree' v))
        && (G.VS.equal (G.preds domtree v) (G.preds domtree' v)))

  let test_all domtree doms name =
    "Domtree " >:::
    [
      sprintf "Same vertices [%s]" name >:: (fun _ ->
        assert_bool "ver" (same_vertices domtree doms));
      sprintf "check_degree [%s]" name >:: (fun _ ->
        assert_bool "deg" (check_degree domtree));
      sprintf "idempotent [%s]" name >:: (fun _ -> 
        assert_bool "idm" (idempotent domtree));
    ]
end

let test_all (func: Ir.Func.t) =
  let root = List.hd_exn func.order in
  let doms = Dom.dominators func in
  let domtree = Dom.submissive_tree doms in
  [
    PosetProperties.test_all doms func.order func.name;
    DomTreeProperties.test_all domtree doms func.name
  ]
