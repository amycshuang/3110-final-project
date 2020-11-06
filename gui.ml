open Graphics 
open Images
open Input
open Player

type block = TallGrass 
           | Water
           | Grass
           | Road
           | Gym
           | PokeCenter
           | House

type box_config =
  { x:int; y:int; w:int; h:int; bw:int;
    b1_col : Graphics.color;
    b2_col : Graphics.color;
    b_col : Graphics.color};;

let gray_outline = Graphics.rgb 229 229 229

let num_of_col = 20 
let num_of_row = 12

let color_of_block = function
  | TallGrass -> Graphics.rgb 57 131 47
  | Water -> Graphics.rgb 128 196 255
  | Grass -> Graphics.rgb 79 227 111
  | Road -> Graphics.rgb 255 230 165
  | Gym -> Graphics.rgb 184 163 163
  | PokeCenter -> Graphics.rgb 251 106 106
  | House -> Graphics.rgb 200 181 255

type box = {
  x: int;
  y: int;
  s: int;
  block_type: block;
  color: Graphics.color;
}

let new_outline (b : box) col = 
  Graphics.set_color col;
  draw_rect b.x b.y b.s b.s

let new_box (b : box) = 
  let x1 = b.x and y1 = b.y in
  Graphics.set_color b.color;
  Graphics.fill_rect x1 y1 b.s b.s;
  new_outline b gray_outline;;

let draw_center (png : string) (b : box) = 
  let img_t = Png.load_as_rgb24 "actual.png" [] in
  let (w, h) = Images.size img_t in 
  let graph_t = Graphic_image.of_image img_t in
  Graphics.draw_image graph_t (b.x + (b.s-w)/2) (b.y + (b.s-h)/2);;

let rec new_grid (lst: block list) s = 
  match lst with 
  | [] -> []
  | b::t -> 
    let n = List.length lst - 1 in
    let px = num_of_col - 1 - (n mod num_of_col) and py = n / num_of_col in 
    let nx = px*(s) and ny = py*(s) in
    let nc = color_of_block b in
    let nb = {x=nx; y=ny; s=s; block_type=b;color=nc} in
    nb::(new_grid t s);;

let new_str_in_box str (b : box) = 
  (* Graphics.set_text_size 100; *)
  let (w, h) = Graphics.text_size str in
  Graphics.moveto (b.x + (b.s-w)/2) (b.y + (b.s-h)/2);
  Graphics.set_color Graphics.black;
  Graphics.draw_string str

let ms_char r b = 
  Graphics.set_color Graphics.black;
  Graphics.fill_circle (b.x + b.s/2) (b.y + b.s/2) r;;

let player_start nrow ncol = ncol * (nrow / 2) + (ncol / 2)

(* let char = Png.load_as_rgb24 "actual.png" [];;
   let char_t = Graphic_image.of_image char;; *)

let array_of_map ncol bl s = Array.of_list (new_grid bl s)

let testing = [Grass;TallGrass;Gym;Water;Grass;
               Grass;TallGrass;Road;Water;Grass;
               Grass;TallGrass;Road;Road;TallGrass;
               PokeCenter;Grass;TallGrass;Road;TallGrass]

let hmm = [Grass; Grass; Grass; Grass; Water; Water; Water; Road;
           TallGrass; Grass; Grass; Grass; Grass; Grass; Grass; Grass;
           Grass; TallGrass; TallGrass; TallGrass; Grass; House;
           Grass; Grass; Water; TallGrass; Water; Road; TallGrass;
           Grass; TallGrass; TallGrass; TallGrass; TallGrass;
           TallGrass; Grass; Grass; TallGrass; TallGrass; TallGrass;
           Grass; Road; House; Grass; Water; Water; Water; Road;
           TallGrass; Grass; TallGrass; TallGrass; TallGrass; Water;
           TallGrass; Grass; Grass; TallGrass; TallGrass; TallGrass;
           TallGrass; Road; Road; Road; Road; Road; Road; Road;
           Road; Road; Grass; TallGrass; Water; Water; Grass; Grass;
           Grass; TallGrass; TallGrass; TallGrass; TallGrass; Road;
           House; House; House; House; TallGrass; TallGrass; Road;
           Road; Road; Road; Road; Road; Road; PokeCenter; PokeCenter;
           Grass; Grass; Grass; TallGrass; Road; Grass; Grass; Grass;
           Grass; TallGrass; TallGrass; Road; Road; Road; Road; Road;
           Road; Road; Road; Road; Road; Road; Road; TallGrass;
           Road; Grass; Grass; Grass; Grass; Water; Water; Gym; Gym;
           Gym; Grass; TallGrass; Road; TallGrass; Road; Grass;
           Grass; Grass; Grass; TallGrass; Road; TallGrass; TallGrass;
           TallGrass; Water; Water; Water; Gym; Gym; Gym; Grass;
           TallGrass; Road; TallGrass; Road; Water; TallGrass;
           TallGrass; TallGrass; TallGrass; Road; TallGrass;
           TallGrass; TallGrass; Water; Water; Water; Grass; Road;
           Grass; Grass; TallGrass; Road; TallGrass; Road; Water;
           TallGrass; TallGrass; TallGrass; TallGrass; Road; Road;
           Road; Road; Road; Road; Road; Road; Road; Road; Road;
           Road; Road; Road; Road; Water; TallGrass; TallGrass;
           TallGrass; TallGrass; TallGrass; TallGrass; TallGrass;
           TallGrass; Road; TallGrass; TallGrass; Grass; Grass; House;
           Grass; Grass; House; Road; Road; Road; Water; Water;
           Water; TallGrass; TallGrass; TallGrass; TallGrass;
           TallGrass; Road; TallGrass; TallGrass; Grass; House; Grass;
           Grass; Grass; Grass; House; Road; Road; Water; Water;
           Water]

let testing_map = array_of_map 5 testing 50

let graph_dims nrow ncol s =
  " " ^ string_of_int (ncol * s) ^ "x" ^ string_of_int (nrow * s)

let big_map = array_of_map 20 (List.rev hmm) 50
let big_map_init = player_start num_of_row num_of_col
let big_map_dims = graph_dims num_of_row num_of_col 50


let move_player (input : Input.input) p = 
  let curr = get_loc p in
  if input.a then set_loc p (curr-1)
  else if input.d then set_loc p (curr+1)
  else if input.w then set_loc p (curr-num_of_col)
  else if input.s then set_loc p (curr+num_of_col)
  else p



let () = Graphics.open_graph big_map_dims;;

let render = 
  let () = clear_graph () in
  (* let _ = Graphics.draw_image g1 0 0 in *)
  let _ = Array.iter new_box big_map in
  (* let _ = draw_center "resized.png" big_map.(10) in *) 
  let _ = ms_char 15 big_map.(big_map_init) in
  (* let _ = ms_char 15 big_map.(get_loc p) in *)
  let () = synchronize () in ()






(* 
  [[Grass;TallGrass;Gym;Water;Grass];
  [Grass;TallGrass;Path;Water;Grass]]
  [Grass;TallGrass;Path;Path;TallGrass]
  [Pokecenter;Grass;tallGrass;Path;TallGrass]]

*)

