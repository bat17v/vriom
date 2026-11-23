open Tsdl_ttf

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

let get_cursor_geometry (f: Ttf.font) (pars: string list list) (i: int) =
    let rec find ls offset j =
        match ls with
        | [] -> (0, offset)
        | first :: rest ->
            match first with
            | [] -> 
                if j = 0
                then (0, offset)
                else find rest (offset + 20) (j - 1)
            | line :: par_rest ->
                let ln = grph_len line in
                if j <= ln
                then (line |> split_to `Grapheme_cluster |> List.take j |> String.concat "" |> px_len f, offset)
                else find (par_rest :: rest) (offset + (Ttf.font_line_skip f)) (j - ln)
    in find pars 0 i
