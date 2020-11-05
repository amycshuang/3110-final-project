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
  { x:int; y:int; w:int; h:int; bw:int; mutable r:relief;
    b1_col : Graphics.color;
    b2_col : Graphics.color;
    b_col : Graphics.color};;

let set_gray x =  (Graphics.rgb x x x);;
let gray1= set_gray 100 and gray2= set_gray 170 and gray3= set_gray 240;;

let draw_box_outline bcf col = 
  Graphics.set_color col;
  draw_rect bcf.x bcf.y bcf.w  bcf.h

let draw_box bcf = 
  let x1 = bcf.x and y1 = bcf.y in
  let x2 = x1+bcf.w and y2 = y1+bcf.h in 
  let ix1 = x1+bcf.bw and ix2 = x2-bcf.bw 
  and iy1 = y1+bcf.bw and iy2 = y2-bcf.bw in 
  let border1 g =
    Graphics.set_color g;
    Graphics.fill_poly 
      [| (x1,y1);(ix1,iy1);(ix2,iy1);(ix2,iy2);(x2,y2);(x2,y1) |] 
  in
  let border2 g = 
    Graphics.set_color g;
    Graphics.fill_poly 
      [| (x1,y1);(ix1,iy1);(ix1,iy2);(ix2,iy2);(x2,y2);(x1,y2) |]
  in
  Graphics.set_color bcf.b_col;
  ( match bcf.r with
      Top  -> 
      Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
      border1 bcf.b1_col; 
      border2 bcf.b2_col
    | Bot  -> 
      Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
      border1 bcf.b2_col; 
      border2 bcf.b1_col
    | Flat -> 
      Graphics.fill_rect x1 y1 bcf.w bcf.h );
  draw_box_outline bcf Graphics.black;;

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

let rec create_grid nb_col n sep b  =
  if n < 0 then []
  else 
    let px = n mod nb_col and py = n / nb_col in
    let nx = b.x +sep + px*(b.w+sep)
    and ny = b.y +sep + py*(b.h+sep) in
    let b1 = {b with x=nx; y=ny} in
    b1::(create_grid nb_col (n-1) sep b);;

let () = Graphics.open_graph "";;

let img1 = Png.load_as_rgb24 "Map.png" [];;
let g1 = Graphic_image.of_image img1;;
let img2 = Png.load_as_rgb24 "actual.png" [];;
let g2 = Graphic_image.of_image img2;;

let vb = 
  let b =  {x=0; y=0; w=50;h=50; bw=2;
            b1_col=gray1; b2_col=gray3; b_col=gray2; r=Top} in 
  Array.of_list (create_grid 5 29 0 b);;

let render = 
  let () = clear_graph () in
  (* let _ = Graphics.draw_image g1 0 0 in 
     let _ = Graphics.draw_image g2 10 10 in *)
  let _ = Array.iter draw_box vb in
  let () = synchronize () in ()


(* 
  [[Grass;TallGrass;Gym;Water;Grass];
  [Grass;TallGrass;Path;Water;Grass]]
  [Grass;TallGrass;Path;Path;TallGrass]]

  | TallGrass 
  | Water
  | Grass
  | Road
  | Gym
  | PokeCenter
  | House

*)

