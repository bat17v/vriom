open Tsdl
open Tsdl_ttf

open Protector

let split_to (b: Uuseg.boundary) (s: string) =
    Uuseg_string.fold_utf_8 b (fun acc elem -> elem :: acc) [] s |> List.rev

let px_len (f: Ttf.font) (s: string) =
    if s = "" then 0 else
    let (x, _) = Result.get_ok (Ttf.size_utf8 f s) in x

let split_to_pars (f: Ttf.font) (s: string) (w: int) =
    Str.split_delim (Str.regexp "\r?\n") s
    |> List.map (split_to `Line_break)
    |> (let rec group_by_w acc rest =
        match rest with
        | [] -> acc
        | elem :: head ->
            match acc with
            | [] -> group_by_w [elem] head
            | last :: other ->
                let x = px_len f (String.trim (last ^ elem)) in
                if x <= w then group_by_w ((last ^ elem) :: other) head
                else (group_by_w (elem :: acc) head)
    in
    List.map (group_by_w []))
    |> List.map List.rev


let grph_len (s: string) = List.length (split_to `Grapheme_cluster s)

let insert_text (s: string) (i: int) (ins: string) =
    let grphs = split_to `Grapheme_cluster s in
    let prefix = List.take i grphs in
    let suffix = List.drop i grphs in
    String.concat "" (prefix @ [ins] @ suffix)

let delete_symbols (s: string) (l: int) (r: int) =
    let grphs = split_to `Grapheme_cluster s in
    let prefix = List.take l grphs in
    let suffix = List.drop r grphs  in
    String.concat "" (prefix @ suffix)


let rec fold_lines (f: Ttf.font) (func: string -> int -> int -> bool -> 'a -> 'a) (init: 'a) ?(offset_x=0) ?(offset_y=0) (pars: string list list) =
    match pars with
    | [] ->
        func "" offset_x offset_y true init
    | [] :: rest_pars ->
        fold_lines f func
            (func "\n" offset_x offset_y false init)
            ~offset_x ~offset_y:(offset_y+20) rest_pars
    | (first_line :: rest_lines) :: rest_pars ->
        fold_lines f func
            (func first_line offset_x offset_y (rest_lines = []) init)
            ~offset_x ~offset_y:(offset_y + (Ttf.font_line_skip f)) (rest_lines :: rest_pars)

let get_cursor_geometry (f: Ttf.font) (pars: string list list) (i: int) =
    let func line ox oy is_last (pref, res) =
        match res with
        | Some _ -> (pref, res)
        | None ->
            if (i - pref) < (grph_len line) || ((i - pref) = (grph_len line) && is_last)
            then (i, Some (line |> split_to `Grapheme_cluster |> List.take (i - pref) |> String.concat "" |> px_len f |> (+) ox, oy))
            else (pref + (grph_len line), None)
    in
    match fold_lines f func (0, None) pars with
    | (_, Some res) -> res
    | (_, None) -> (0, 0)

let draw_pars (renderer: Sdl.renderer) (f: Ttf.font) (color: Sdl.color) (pars: string list list) (offset_x: int) (offset_y: int) =
    let func line ox oy _ () =
        if line = "" || line = "\n" then ()
        else
        let@$ surface = Result.get_ok (Ttf.render_utf8_blended f line color) in
        let@! texture = Result.get_ok (Sdl.create_texture_from_surface renderer surface) in
        let (s_w, s_h) = Sdl.get_surface_size surface in

        let rect = Sdl.Rect.create ~x:ox ~y:oy ~w:s_w ~h:s_h in
        let _ = Sdl.render_copy ~dst:rect renderer texture in ()
    in
    fold_lines f func () ~offset_x ~offset_y pars

