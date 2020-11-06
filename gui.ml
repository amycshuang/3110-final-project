open Graphics 
open Images

type relief = Top | Bot | Flat;;
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


let set_gray x =  (Graphics.rgb x x x);;
let gray1= set_gray 100 and gray2= set_gray 170 and gray3= set_gray 240;;
let gray_outline = Graphics.rgb 229 229 229

let color_of_block = function
  | TallGrass -> Graphics.rgb 57 131 47
  | Water -> Graphics.rgb 128 196 255
  | Grass -> Graphics.rgb 79 227 111
  | Road -> Graphics.rgb 255 230 165
  | Gym -> Graphics.rgb 184 163 163
  | PokeCenter -> Graphics.rgb 251 106 106
  | House -> Graphics.rgb 200 181 255

let draw_box_outline bcf col = 
  Graphics.set_color col;
  draw_rect bcf.x bcf.y bcf.w bcf.h

let draw_box bcf = 
  let x1 = bcf.x and y1 = bcf.y in
  Graphics.set_color bcf.b_col;
  Graphics.fill_rect x1 y1 bcf.w bcf.h;
  draw_box_outline bcf gray_outline;;

type position = Left | Center | Right 

let draw_string_in_box pos str bcf col = 
  let (w, h) = Graphics.text_size str in
  let ty = bcf.y + (bcf.h-h)/2 in 
  ( match pos with 
      Center -> Graphics.moveto (bcf.x + (bcf.w-w)/2) ty 
    | Right  -> let tx = bcf.x + bcf.w - w - bcf.bw - 1 in 
      Graphics.moveto tx ty 
    | Left   -> let tx = bcf.x + bcf.bw + 1 in Graphics.moveto tx ty  );
  Graphics.set_color col;
  Graphics.draw_string str



let rec create_grid nb_col n b =
  if n < 0 then []
  else 
    let px = n mod nb_col and py = n / nb_col in
    let nx = b.x + px*(b.w)
    and ny = b.y + py*(b.h) in
    let b1 = {b with x=nx; y=ny} in
    b1::(create_grid nb_col (n-1) b);;

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

let rec new_grid ncol (lst: block list) s = 
  match lst with 
  | [] -> []
  | b::t -> 
    let n = List.length lst - 1 in
    let px = n mod ncol and py = n / ncol in 
    let nx = px*(s) and ny = py*(s) in
    let nc = color_of_block b in
    let nb = {x=nx; y=ny; s=s; block_type=b;color=nc} in
    nb::(new_grid ncol t s);;

let new_str_in_box str (b : box) = 
  (* Graphics.set_text_size 100; *)
  let (w, h) = Graphics.text_size str in
  Graphics.moveto (b.x + (b.s-w)/2) (b.y + (b.s-h)/2);
  Graphics.set_color Graphics.black;
  Graphics.draw_string str

let ms_char r b = 
  Graphics.set_color Graphics.black;
  Graphics.fill_circle (b.x + b.s/2) (b.y + b.s/2) r;;

(* let char = Png.load_as_rgb24 "actual.png" [];;
   let char_t = Graphic_image.of_image char;; *)

let vb = 
  let b =  {x=0; y=0; w=50;h=50; bw=2;
            b1_col=gray1; b2_col=gray3; b_col=gray2} in 
  Array.of_list (create_grid 5 29 b);;

let array_of_map ncol bl s = Array.of_list (new_grid ncol bl s)

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
let big_map = array_of_map 20 (List.rev hmm) 50

let () = Graphics.open_graph " 1000x600";;

let render = 
  let () = clear_graph () in
  (* let _ = Graphics.draw_image g1 0 0 in *)
  let _ = Array.iter new_box big_map in
  (* let _ = draw_center "resized.png" big_map.(10) in 
     let _ = new_str_in_box "X" big_map.(10) in *)
  let _ = ms_char 15 big_map.(10) in
  let () = synchronize () in ()





(* 
  [[Grass;TallGrass;Gym;Water;Grass];
  [Grass;TallGrass;Path;Water;Grass]]
  [Grass;TallGrass;Path;Path;TallGrass]
  [Pokecenter;Grass;tallGrass;Path;TallGrass]]

*)

