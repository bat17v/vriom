open Tsdl

let with_object destoyer obj func =
    Fun.protect
        ~finally:(fun () -> destoyer obj; ())
        (fun () -> func obj)

let ( let@$ ) obj func = with_object Sdl.free_surface obj func

let ( let@! ) obj func = with_object Sdl.destroy_texture obj func
