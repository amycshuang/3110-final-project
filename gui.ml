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

type text = {
  text: string;
  color: Graphics.color;
}

type text_box = {
  x : int;
  y : int;
  size: int;
  text : string;
  text_color: Graphics.color;
}

let draw_text (b: text_box) =
  Graphics.set_color b.text_color;
  Graphics.set_text_size b.size;
  Graphics.moveto (b.x) (b.y);
  Graphics.draw_string b.text

let menu_lst = [{text = "Pokemon"; color = black}; {text = "Bag"; color = black};
                {text = "Battle"; color = black}]

let opening_box = {
  x =  500;
  y = 500;
  size = 100;
  text = "Welcome to Pokaml";
  text_color = black;
}

let () = set_window_title "Pokaml";;


(* let menu_screen = array_of_text 100 menu_lst  *)

(* let render_opening_screen = 
   let () = clear_graph () in 
   let () = draw_text opening_box in 
   let () = Array.iter draw_text menu_screen in  () *)

(* let draw_outline (b : box) col = 
   Graphics.set_color col;
   draw_rect b.x b.y b.s b.s *)

let rec draw_menu texts s_x s_y = 
  Graphics.moveto s_x s_y;
  match texts with 
  | [] -> ()
  | h :: t -> begin 
      let x = s_x in 
      let y = box_len + s_y in 
      Graphics.set_color h.color;
      Graphics.draw_string h.text;
      draw_menu t x y
    end 

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
  let () = draw_menu menu_lst 100 (Array.length s + 50) in 
  (* let () = draw_char 15 big_map.(big_map_init) in *)
  (* let _ = ms_char 15 big_map.(get_loc p) in *)
  let () = synchronize () in ()

let () = render trying test_player
