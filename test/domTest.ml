open! Core
open OUnit2
open Printf
open Cfg

module G = Dominance.G

type t = Dominance.t

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
      not(String.equal b s) && G.VS.mem (G.succs doms s) b in
    b |> G.succs doms |> G.VS.exists ~f:predicate |> not

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


let test_all func =
  let doms = Dominance.dominators func in
  PosetProperties.test_all doms func.order func.name
