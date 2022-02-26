open Graph.Graphviz.DotAttributes
open Printf

module SM = Core.String.Map
module SS = Core.String.Set

let g_attrs ~label _ =
  [ `Label label;
    `Fontname "Times";
    `Fontsize 20; ]

let v_attrs label =
  [ `Label label;
    `Fontname "Times";
    `Fontsize 16; ]

let default_v_attrs _ =
  v_attrs "N/A"

let default_e_attrs _ =
  [ `Dir `Forward; ]

let vertex_name = sprintf "\"%s\""

let get_subgraph = fun _ -> None
