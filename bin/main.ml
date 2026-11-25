open Bogue
open Tsdl
open Tsdl_ttf

module W = Widget
module L = Layout

open Vriom

let _ = Ttf.init ()

let font = Result.get_ok (Ttf.open_font (Option.get (Theme.get_font_path_opt "Ubuntu-R.ttf")) 20)

type message =
    | Enter
    | Backspace
    | Right
    | Left

let hotkey_pad = Hotkeys.[
    Hotkey.create Enter ~code:Sdl.K.return ();
    Hotkey.create Backspace ~code:Sdl.K.backspace ();
    Hotkey.create Right ~code:Sdl.K.right ();
    Hotkey.create Left ~code:Sdl.K.left ();
]

let create_widget () =
    let buffer = ref "" in
    let cursor_pos = ref 0 in
    let area_widget = W.sdl_area ~w:400 ~h:200 () in
    let draw renderer =
        let () = Sdl.start_text_input () in
        let _ = Sdl.set_render_draw_color renderer 255 255 255 255 in 
        let _ = Sdl.render_clear renderer in
        let rect = Sdl.Rect.create ~x:10 ~y:10 ~w:380 ~h:180 in
        let _ = Sdl.set_render_draw_color renderer 255 0 0 255 in
        let _ = Sdl.render_draw_rect renderer (Some rect) in

        let ls = Text.split_to_pars font !buffer 360 in
        let color = Sdl.Color.create ~r:255 ~g:0 ~b:0 ~a:255 in
        let () = Text.draw_pars renderer font color ls 20 20 in

        let (x, y) = Text.get_cursor_geometry font ls !cursor_pos in
        let rect = Sdl.Rect.create ~x:(20 + x) ~y:(20 + y) ~w:2 ~h:(Ttf.font_line_skip font) in
        let _ = Sdl.set_render_draw_color renderer 0 0 255 255 in
        let _ = Sdl.render_draw_rect renderer (Some rect) in
        ()
    in
    let area = W.get_sdl_area area_widget in
    let on_text_input _ _ ev =
        let text = Sdl.Event.(get ev text_input_text) in
        buffer := Text.insert_text !buffer !cursor_pos text;
        cursor_pos := !cursor_pos + 1;
        Sdl_area.update area;
        W.update area_widget
    in
    let on_hotkey msg =
        match msg with
        | Enter ->
            buffer := Text.insert_text !buffer !cursor_pos "\n";
            cursor_pos := !cursor_pos + 1;
        | Backspace ->
            if !cursor_pos > 0 then begin
            buffer := Text.delete_symbols !buffer (!cursor_pos - 1) !cursor_pos;
            cursor_pos := !cursor_pos - 1;
            end else ()
        | Left ->
            cursor_pos := (max 0 (!cursor_pos - 1));
        | Right ->
            cursor_pos := (min (Text.grph_len !buffer) (!cursor_pos + 1));
        ;
        Sdl_area.update area;
        W.update area_widget
    in
    let () = Sdl_area.add area draw in
    let conn_input = W.connect_main area_widget area_widget on_text_input Trigger.[text_input] in
    let conn_hotkeys = W.connect_main area_widget area_widget (Hotkeys.get_solver hotkey_pad on_hotkey) Trigger.[key_down] in
    area_widget, [conn_input; conn_hotkeys]

let () =
    let (widget, connects) = create_widget () in
    let layout = L.resident ~keyboard_focus:true widget in
    let board = Bogue.of_layout ~connections:connects layout in
    Bogue.run board
