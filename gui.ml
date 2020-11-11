open Player
open Graphics 
open Input

type block = TallGrass 
           | Water
           | Grass
           | Road
           | Gym
           | PokeCenter
           | House

type text = {
  text: string;
  color: Graphics.color;
}

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

let new_outline (b : box) col = 
  Graphics.set_color col;
  draw_rect b.x b.y b.s b.s

let new_box (b : box) = 
  let x1 = b.x and y1 = b.y in
  Graphics.set_color b.color;
  Graphics.fill_rect x1 y1 b.s b.s;
  new_outline b gray_outline;;

let rec text_grid s (lst : text list) = 
  match lst with 
  | [] -> []
  | h :: t -> begin 
      let n = List.length lst - 1 in 
      let px =  num_of_col - 1 - (n mod num_of_col) and py = (n / num_of_col) + 1000 in 
      let nx = px*s + 100000 and ny = py*s + 500 in
      let new_tbox = {
        x = nx;
        y = ny;
        size = s;
        text = h.text;
        text_color = h.color;
      } in 
      new_tbox :: (text_grid s t)
    end 

let rec new_grid (lst: block list) s = 
  match lst with 
  | [] -> []
  | b::t -> begin
      let n = List.length lst - 1 in
      let px = num_of_col - 1 - (n mod num_of_col) and py = n / num_of_col in 
      let nx = px*(s) and ny = py*(s) in
      let nc = color_of_block b in
      let nb = {x=nx; y=ny; s=s; block_type=b;color=nc} in
      nb::(new_grid t s)
    end

let ms_char r (b: box) = 
  Graphics.set_color Graphics.black;
  Graphics.fill_circle (b.x + b.s/2) (b.y + b.s/2) r;;

let player_start nrow ncol = ncol * (nrow / 2) + (ncol / 2)

(* let char = Png.load_as_rgb24 "actual.png" [];;
   let char_t = Graphic_image.of_image char;; *)

let array_of_map ncol bl s = Array.of_list (new_grid bl s)
let array_of_text s tl = Array.of_list (text_grid s tl)

let menu_lst = [{text = "Pokemon"; color = black}; {text = "Bag"; color = black};
                {text = "Battle"; color = black}; {text = "Dummy Var"; color = black}]

let opening_box = {
  x =  500;
  y = 500;
  size = 100;
  text = "Welcome to Pokaml";
  text_color = black;
}

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


let graph_dims nrow ncol s =
  " " ^ string_of_int (ncol * s) ^ "x" ^ string_of_int (nrow * s + 230)

let menu_screen = array_of_text 100 menu_lst 

let big_map = array_of_map 20 (List.rev hmm) 50
let big_map_init = player_start num_of_row num_of_col
let big_map_dims = graph_dims num_of_row num_of_col 50

let move_map p m =
  let curr = get_loc p in
  match m with
  | Some Up -> set_loc p (curr - num_of_col)
  | Some Left -> set_loc p (curr - 1)
  | Some Down -> set_loc p (curr+num_of_col)
  | Some Right -> set_loc p (curr + 1)
  | None -> p

let () = Graphics.open_graph big_map_dims;;
let () = set_window_title "Pokaml";;

let render_opening_screen = 
  let () = clear_graph () in 
  let () = draw_text opening_box in ()

(* let render = 
   let () = clear_graph () in
   (* let _ = Graphics.draw_image g1 0 0 in *)
   let _ = Array.iter new_box big_map in
   (* let _ = draw_center "resized.png" big_map.(10) in *) 
   let _ = ms_char 15 big_map.(big_map_init) in
   let () = Array.iter draw_text menu_screen in 
   (* let _ = ms_char 15 big_map.(get_loc p) in *)
   let () = synchronize () in () *)



(** draw more functions and call in render *)


(* 
  [[Grass;TallGrass;Gym;Water;Grass];
  [Grass;TallGrass;Path;Water;Grass]]
  [Grass;TallGrass;Path;Path;TallGrass]
  [Pokecenter;Grass;tallGrass;Path;TallGrass]]

*)

