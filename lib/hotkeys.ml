open Tsdl

let hasmod (keymod: int) (pattern: int) =
    (keymod land pattern) <> 0

module Hotkey = struct
    type 'a t = {
        ctrl: bool;
        alt: bool;
        shift: bool;

        code: int;
        msg: 'a;
    }

    let create msg ~code ?(shift=false) ?(ctrl=false) ?(alt=false) () =
        { shift; ctrl; alt; code; msg }

    let is_solve (hotkey: 'a t) (keycode: int) (keymod: int) =
        keycode = hotkey.code
        && ((hotkey.ctrl) = (hasmod keymod Sdl.Kmod.ctrl))
        && ((hotkey.alt) = (hasmod keymod Sdl.Kmod.alt))
        && ((hotkey.shift) = (hasmod keymod Sdl.Kmod.shift))
end


let rec solve_hotkey_list (rest: 'a Hotkey.t list) (keycode: int) (keymod: int) =
    match rest with
    | [] -> None
    | first :: other ->
        if Hotkey.is_solve first keycode keymod
        then Some first.msg
        else solve_hotkey_list other keycode keymod

let get_solver (pad: 'a Hotkey.t list) (callback: 'a -> unit) =
    let solver _ _ ev =
        let keycode = Sdl.Event.(get ev keyboard_keycode) in
        let keymod = Sdl.Event.(get ev keyboard_keymod) in
        match solve_hotkey_list pad keycode keymod with
        | None -> ()
        | Some msg -> callback msg
    in solver
        
