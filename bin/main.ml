open Bogue
module W = Widget
module L = Layout

let text_area ?(h=100) ?(w=100) () =
    let w = W.empty ~h ~w () in
    L.resident w

let () = ()
