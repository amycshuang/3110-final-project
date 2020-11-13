open Player
open Graphics 
open State
open Block
open Pokemon

let box_len = 25
let char_size = 8
let char_color = Graphics.black
let panel_height = 75
let panel_color = Graphics.rgb 151 199 218
let panel_outline = 3

type option = {
  loc : int * int;
  text : string;
  selected: bool;
}

let battle = {loc = (45, 80); text = "Battle"; selected = false}
let bag = {loc = (145, 80); text = "Bag"; selected = false}
let pokemon = {loc = (245, 80); text = "Pokemon"; selected = false}
let run = {loc = (395, 80); text = "Run"; selected = false}
let battle_options = [battle; bag; pokemon; run]

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
        (row * box_len + panel_height) box_len box_len;
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
  let (x, y) = p.location in
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

let render_walk (st : State.state) = 
  let () = Graphics.open_graph (graph_dims st.map); in 
  let () = clear_graph () in
  let () = draw_map st.map in
  let () = draw_panel st.map in
  let () = draw_char st.player in
  let () = display_text st.panel_txt in
  let () = synchronize () in ()
(* 
let draw_choose (dir : State.move) = 
  match dir with 
  | Some Left -> ()
  | Some Right -> ()
  | None -> () *)

(* let render_pkm_stats pkm s_x s_y = 
   let () = moveto s_x s_y in 
   let () = draw_string (get_name pkm) in 
   let () = moveto s_x (s_y - 20) in 
   let () = draw_string ("lvl: " ^ string_of_int (get_level pkm)) in 
   let () = moveto s_x (s_y - 40) in 
   let () = draw_string ("hp: " ^ string_of_int (get_hp pkm)) in () *)

(* 
let render_pkm x y color = 
  Graphics.set_color color;
  Graphics.fill_circle x y 40;;

let rec pkm_mvs_string = function
  | [] -> ""
  | h :: t -> h ^ "   " ^ pkm_mvs_string t

let rec render_battle_options color b_opts = 
  Graphics.set_color color;
  match b_opts with 
  | [] -> () 
  | h :: t -> begin 
      let (x, y) = h.loc in 
      let () = moveto x y in 
      let () = draw_string h.text in 
      render_battle_options color t
    end *)

(* let render_encounter (st : State.state) (block : Block.block)
    (trn_pkm : Pokemon.t) (w_pkm : Pokemon.t) =
   let w_width = size_x () in 
   let w_height = size_y () in 
   let () = Graphics.clear_graph () in 
   let s_x = (w_width / 4) * 3 + 20 in 
   let s_y = (w_height / 4) in 
   let () = render_pkm_stats trn_pkm s_x (s_y + 80) in 
   let () = render_pkm_stats w_pkm 45 (s_y * 3 + 40) in 
   let () = render_pkm 130 (s_y + 70) blue in 
   let () = render_pkm (s_x - 20) (s_y * 3 + 20) magenta in
   let () = render_battle_options black battle_options in  
   let () = synchronize () in () *)


(* let render_battle = failwith "unimplemented"  *)
(* 
let render_building (st : State.state) (block : State.block) =
  match block with 
  | PokeCenter -> 
  | Gym -> 
  | House ->  *)

(* let render_win = failwith "unimplemented"  *)

let battle_panel_ht = int_of_float (1.5 *. float_of_int panel_height)

let custom_outline color x1 y1 x2 y2 buffer size = 
  Graphics.set_color color;
  Graphics.set_line_width size;
  Graphics.draw_rect (x1 + buffer) (y1 + buffer) (x2 - 2 * buffer) (y2 - 2 * buffer)

let bottom_bg_panel () = 
  let panel_width = size_x () in
  Graphics.set_color (Graphics.rgb 40 80 104);
  Graphics.fill_rect 0 0 panel_width battle_panel_ht;
  custom_outline Graphics.white 0 0 panel_width battle_panel_ht 13 5;
  custom_outline (Graphics.rgb 200 168 72) 0 0 panel_width battle_panel_ht 5 12;
  custom_outline Graphics.black 0 0 panel_width battle_panel_ht 0 5

let bottom_menu_panel () =
  let panel_width = size_x () in
  let new_x = panel_width / 2 in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect new_x 0 panel_width battle_panel_ht;
  custom_outline (Graphics.rgb 117 111 144) new_x 0 panel_width battle_panel_ht 5 12;
  custom_outline Graphics.black new_x 0 panel_width battle_panel_ht 0 5

let draw_bottom_panel () = 
  bottom_bg_panel ();
  bottom_menu_panel ()

let encounter_menu = ["FIGHT"; "BAG"; "POKEMON"; "RUN"]

let make_options ncol x y width height lst =
  let s_x = width / ncol in 
  let s_y = height / ((List.length lst) / ncol ) in
  let rec making acc = function
    | [] -> acc
    | h :: t -> let n = (List.length t) + 1 in
      let multx = n mod ncol in
      let multy = (n + 1) / ncol - 1 in
      let button = {loc=((multx * s_x + (s_x / 3) + x),
                         ((multy * s_y) + (s_y /3 + y))); text=h; 
                    selected=false} in making (button::acc) t in
  making [] lst

(* let rec make_options lst x y width height = 
   let n_w = width / 2 in
   let n_h = height / (List.length lst) in
   match lst with 
   | [] -> []
   | h::t -> 
    let n = (List.length lst) - 1 in
    let multx = (n + 1) mod 2 in 
    let nx = multx * (n_w) in
    let nb = {loc=(x + nx, y + n_h); text=h; selected=false} in
    nb::(make_options t x y width height);; *)

let list_of_stats pokemon = 
  [(String.uppercase_ascii pokemon.name);
   "Lv" ^ string_of_int pokemon.stats.level]




let opt_lst () = make_options 2 (size_x () / 2) 0 (size_x () / 2) battle_panel_ht encounter_menu

let rec draw_options = function
  | [] -> ()
  | opt :: t -> let (x, y) = opt.loc in
    Graphics.moveto x y;
    Graphics.set_color Graphics.black;
    Graphics.draw_string opt.text;
    draw_options t

let poke_panel x y = 
  let width = size_x () / 3 in
  let height = size_y () / 5 in
  Graphics.set_color (Graphics.rgb 248 248 216);
  Graphics.fill_rect x y width height;
  Graphics.set_color (Graphics.rgb 58 82 52);
  Graphics.set_line_width 5;
  Graphics.draw_rect x y width height

let render_encounter (st : State.state) =
  let () = Graphics.open_graph (graph_dims st.map); in
  let () = Graphics.clear_graph () in 
  let () = draw_bottom_panel () in
  let () = poke_panel 50 275 in
  let () = poke_panel 300 150 in
  let () = draw_options (opt_lst ()) in
  let () = synchronize () in ()

let () = render_encounter testing_state





