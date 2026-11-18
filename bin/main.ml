open Bogue
module W = Widget
module L = Layout

let () =
  (* 1. Создаём текстовый блок абзаца *)
  let text_w = W.text_display ~w:560 ~h:60 "Дождь стучал по крыше — ровно, как метроном." in
  let text_box = L.resident ~w:560 ~h:60 text_w in

  (* 2. Создаём немигающий курсор как тонкую тёмную полоску *)
  let cursor_w = W.empty ~w:2 ~h:20 () in
  let cursor_bg = Style.(create ~background:(color_bg Draw.(opaque (find_color "#1a1a1a"))) ()) in
  let cursor_box = L.resident ~w:2 ~h:20 ~background:(L.style_bg cursor_bg) cursor_w in

  (* Сдвигаем курсор в нужную позицию поверх текста *)
  L.setx cursor_box 375;
  L.sety cursor_box 4;

  (* 3. Накладываем текст и курсор друг на друга внутри цветной плашки абзаца *)
  let para_bg = Style.(create ~background:(color_bg Draw.(opaque (find_color "#eef4fa"))) ()) in
  let paragraph = L.superpose ~w:580 ~h:70 ~background:(L.style_bg para_bg) [ text_box; cursor_box ] in

  (* 4. Запускаем окно *)
  let board = Bogue.of_layout paragraph in
  Bogue.run board
