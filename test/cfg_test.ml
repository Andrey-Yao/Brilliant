open OUnit2
open Cfg.Util
open Cfg

module DomTest = struct
  
  type t = Dominance.t

  let find doms b = match SM.find doms b with
    | None -> SS.empty
    | Some s -> s

  module PosetProperties = struct

    (**Forall [b], [b] in [doms[b]]*)
    let reflexive_single (doms: t) b =
      SS.mem (find doms b) b

    (**Forall [b], [s] in [doms[b]] & [b] in [doms[s]]
       => [s] = [b]*)
    let antisymmetric_single (doms: t) b =
      let predicate s =
        not(String.equal b s) && SS.mem (find doms s) b in
      b |> find doms |> SS.exists ~f:predicate |> not

    (**Forall [s] in [doms[b]], [doms[s] subseteq [doms[b]]]*)
    let transitive_single (doms: t) b =
      let doms_b = find doms b in
      let predicate s =
        s |> find doms |> SS.is_subset ~of_:doms_b in
      doms_b |> SS.for_all ~f:predicate

    let poset_single (doms: t) b =
      reflexive_single doms b |> assert_bool "reflexivity";
      transitive_single doms b |> assert_bool "transitivity";
      antisymmetric_single doms b |> assert_bool "antisymmetry"

    let poset_test blocks doms =
      List.iter (fun b -> poset_single doms b) blocks
  end


  module GraphProperties = struct

    let children doms b = SS.remove (find doms b) b


  end


    
end
