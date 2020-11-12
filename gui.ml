open Player
open Graphics 
open State

let box_len = 25
let char_size = 8
let char_color = Graphics.black
let panel_height = 75
let panel_color = Graphics.rgb 151 199 218
let panel_outline = 3

let color_of_block b = 
  match b with
  | TallGrass -> Graphics.rgb 88 165 116
  | Water -> Graphics.rgb 152 200 240
  | Grass -> Graphics.rgb 112 200 160
  | Road -> Graphics.rgb 232 224 136
  | Gym -> Graphics.rgb 192 168 103
  | PokeCenter -> Graphics.rgb 216 104 96
  | House -> Graphics.rgb 200 168 240

let draw_map blocks =
  let ncol = Array.length blocks.(0) in
  let nrow = Array.length blocks in
  for row = 0 to (nrow - 1) do
    for col = 0 to (ncol - 1) do
      Graphics.set_color (color_of_block (blocks.(row)).(col));
      Graphics.fill_rect (col * box_len)
        ((nrow - row - 1) * box_len + panel_height) box_len box_len;
    done
  done

let draw_panel blocks = 
  let panel_width = (Array.length blocks.(0)) * box_len in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 panel_width panel_height;
  Graphics.set_color panel_color;
  Graphics.set_line_width panel_outline;
  Graphics.draw_rect 0 0 panel_width panel_height

let draw_char p = 
  let (x, y) = get_loc p in
  Graphics.set_color char_color;
  Graphics.fill_circle (x * box_len + box_len/2)
    (y * box_len + box_len/2 + panel_height) char_size;;

let graph_dims blocks =
  let ncol = Array.length blocks.(0) in
  let nrow = Array.length blocks in
  " " ^ string_of_int (ncol * box_len) ^ "x" ^
  string_of_int (nrow * box_len + panel_height)

let display_text s =
  Graphics.moveto 10 50;
  Graphics.draw_string s

let render (st : State.state) = 
  let () = Graphics.open_graph (graph_dims st.map); in 
  let () = clear_graph () in
  let () = draw_map st.map in
  let () = draw_panel st.map in
  let () = draw_char st.player in
  let () = display_text st.panel_txt in
  let () = synchronize () in ()

let () = render testing_state