open Tsdl
open Tsdl_ttf

open Protector

let split_to (b: Uuseg.boundary) (s: string) =
    Uuseg_string.fold_utf_8 b (fun acc elem -> elem :: acc) [] s |> List.rev

let px_len (f: Ttf.font) (s: string) =
    if s = "" then 0 else
    let (x, _) = Result.get_ok (Ttf.size_utf8 f s) in x

let px_len_pref (f: Ttf.font) (s: string) (ln: int) =
    s |> split_to `Grapheme_cluster |> List.take ln |> String.concat "" |> px_len f

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
    | [] -> init
    | [] :: rest_pars ->
        fold_lines f func
            (func "\n" offset_x offset_y false init)
            ~offset_x ~offset_y:(offset_y+20) rest_pars
    | (first_line :: rest_lines) :: rest_pars ->
        if String.take_last 1 first_line = " " then
            let prefix = String.drop_last 1 first_line in
            let acc2 = func prefix offset_x offset_y true init in
            let acc3 = func " " (offset_x + px_len f prefix) offset_y (rest_lines = []) acc2 in
            fold_lines f func acc3 ~offset_x ~offset_y:(offset_y + (Ttf.font_line_skip f)) (rest_lines :: rest_pars)
        else
            let acc2 = func first_line offset_x offset_y true init in
            fold_lines f func acc2 ~offset_x ~offset_y:(offset_y + (Ttf.font_line_skip f)) (rest_lines :: rest_pars)

let get_cursor_geometry (f: Ttf.font) (pars: string list list) (i: int) =
    let func line ox oy is_visible (pref, res_opt) =
        match res_opt with
        | Some _ -> (pref, res_opt)
        | None ->
            if (i - pref) < (grph_len line) || ((i - pref) = (grph_len line) && is_visible)
            then (i, Some (ox + px_len_pref f line (i - pref), oy))
            else (pref + (grph_len line), None)
    in
    match fold_lines f func (0, None) pars with
    | (_, Some res) -> res
    | (_, None) -> (0, 0)

let draw_pars (renderer: Sdl.renderer) (f: Ttf.font) (color: Sdl.color) (offset_x: int) (offset_y: int) (pars: string list list) =
    let func line ox oy is_visible () =
        if line = "" || not is_visible then ()
        else
            let@$ surface = Result.get_ok (Ttf.render_utf8_blended f line color) in
            let@! texture = Result.get_ok (Sdl.create_texture_from_surface renderer surface) in
            let (s_w, s_h) = Sdl.get_surface_size surface in

            let rect = Sdl.Rect.create ~x:ox ~y:oy ~w:s_w ~h:s_h in
            let _ = Sdl.render_copy ~dst:rect renderer texture in ()
    in
    fold_lines f func () ~offset_x ~offset_y pars

let find_cursor_pos (f: Ttf.font) (pars: string list list) (x: int) (y: int) =
    let pt = Sdl.Point.create ~x ~y in
    let rec find_in_line line ox l r =
        if r - l <= 1 then r
        else
            let m = (l + r) / 2 in
            if (px_len_pref f line (m + 1) + px_len_pref f line m) / 2 > ox
            then find_in_line line ox l m
            else find_in_line line ox m r
    in
    let func line ox oy is_visible (pref, res) =
        let newpref = pref + (grph_len line) in

        if not is_visible then (newpref, res)
            else
            let (w, h) = Result.get_ok (Ttf.size_utf8 f line) in
            let rect = Sdl.Rect.create ~x:ox ~y:oy ~w ~h in
            if Sdl.point_in_rect pt rect
                then (newpref, pref + find_in_line line (x - ox) (-1) (grph_len line))
                else if y > oy + h || (y > oy && x > ox)
                    then (newpref, newpref)
                    else (newpref, res)
    in
    let (_, cursor_pos) = fold_lines f func (0, 0) pars in
    cursor_pos

