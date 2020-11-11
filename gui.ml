open Player
open Graphics 
open State

let box_len = 50
let char_size = 10

let color_of_block b = 
  match b with
  | TallGrass -> Graphics.rgb 88 165 116
  | Water -> Graphics.rgb 112 184 240
  | Grass -> Graphics.rgb 112 200 160
  | Road -> Graphics.rgb 232 224 136
  | Gym -> Graphics.rgb 192 168 103
  | PokeCenter -> Graphics.rgb 216 104 96
  | House -> Graphics.rgb 200 168 240

(* let draw_outline (b : box) col = 
   Graphics.set_color col;
   draw_rect b.x b.y b.s b.s *)

let draw_map blocks =
  let ncol = Array.length blocks.(0) in
  let nrow = Array.length blocks in
  for row = 0 to (nrow - 1) do
    for col = 0 to (ncol - 1) do
      Graphics.set_color (color_of_block (blocks.(row)).(col));
      Graphics.fill_rect (col * box_len) ((nrow - row - 1) * box_len) 
        box_len box_len;
    done
  done

let draw_char p = 
  let (x, y) = get_loc p in
  Graphics.set_color Graphics.black;
  Graphics.fill_circle (x * box_len + box_len/2) (y * box_len + box_len/2) 
    char_size;;

let graph_dims blocks =
  let ncol = Array.length blocks.(0) in
  let nrow = Array.length blocks in
  " " ^ string_of_int (ncol * box_len) ^ "x" ^ string_of_int (nrow * box_len)

let render (s : map) p = 
  let () = Graphics.open_graph (graph_dims s); in 
  let () = clear_graph () in
  let () = draw_map s in
  let () = draw_char p in
  (* let () = draw_char 15 big_map.(big_map_init) in *)
  (* let _ = ms_char 15 big_map.(get_loc p) in *)
  let () = synchronize () in ()

let () = render trying test_player