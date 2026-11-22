open Tsdl_ttf

let split_to_tokens (s: string) =
    Uuseg_string.fold_utf_8 `Line_break (fun acc elem -> elem :: acc) [] s |> List.rev

let px_len f (s: string) =
    if s = "" then 0 else
    let (x, _) = Result.get_ok (Ttf.size_utf8 f s) in x

let split_to_pars (f) (s: string) (w: int) =
    Str.split_delim (Str.regexp "\r?\n") s
    |> List.map split_to_tokens
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
