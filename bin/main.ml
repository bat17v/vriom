open Bogue
open Tsdl
open Tsdl_ttf
module W = Widget
module L = Layout

let _ = Ttf.init ()

let font = Result.get_ok (Ttf.open_font (Option.get (Theme.get_font_path_opt "Ubuntu-R.ttf")) 20)

let create_widget () =
    let buffer = ref "!" in
    let area_widget = W.sdl_area ~w:400 ~h:200 () in
    let draw renderer =
        let () = Sdl.start_text_input () in
        let _ = Sdl.set_render_draw_color renderer 255 255 255 255 in 
        let _ = Sdl.render_clear renderer in
        let rect = Sdl.Rect.create ~x:10 ~y:10 ~w:380 ~h:180 in
        let _ = Sdl.set_render_draw_color renderer 255 0 0 255 in
        let _ = Sdl.render_draw_rect renderer (Some rect) in

        let color = Sdl.Color.create ~r:255 ~g:0 ~b:0 ~a:255 in
        let y = ref 20 in
        let draw_text text =
            let surface = Result.get_ok (Ttf.render_utf8_blended font text color) in
            let texture = Result.get_ok (Sdl.create_texture_from_surface renderer surface) in
            let (w, h) = Sdl.get_surface_size surface in
            
            let rect = Sdl.Rect.create ~x:20 ~y:!y ~w ~h in
            let _ = Sdl.render_copy ~dst:rect renderer texture in
            let _ = (y := !y + (Ttf.font_line_skip font)) in
            ()
        in
        let ls = [!buffer] in
        let () = List.iter draw_text ls in
        ()
    in
    let area = W.get_sdl_area area_widget in
    let on_text_input _ _ ev =
        let text = Sdl.Event.(get ev text_input_text) in
        buffer := !buffer ^ text;
        Sdl_area.update area;
        W.update area_widget
    in
    let () = Sdl_area.add area draw in
    let conn_input = W.connect_main area_widget area_widget on_text_input Trigger.[text_input] in
    area_widget, [conn_input]

let () =
    let (widget, connects) = create_widget () in
    let layout = L.resident widget in
    let board = Bogue.of_layout ~connections:connects layout in
    Bogue.run board
