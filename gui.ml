open Player
open Graphics 
open State
open Block
open Pokemon
open Encounter
open Menu

(** [box_len] is the size of each block on the map *)
let box_len = 25

(** [char_size] is the size of the character on the map *)
let char_size = 8

(** [char_color] is the color of the character on the map *)
let char_color = Graphics.black

(** [panel_height] is the height of the text panel on the bottom of the screen 
    while player is walking *)
let panel_height = 75

(** [panel_color] is the color of the text panel on the bottom of the screen 
    while player is walking *)
let panel_color = Graphics.rgb 151 199 218

(** [panel_outline] is the border size of the text panel on the bottom of the 
    screen while player is walking*)
let panel_outline = 3

(** The type representing a button option on the text panel *)
type option = {
  loc : int * int;
  text : string;
  selected: bool;
}

(** [color_of_block b] maps a block type to the specified color on the map *)
let color_of_block b = 
  match b with
  | TallGrass -> Graphics.rgb 88 165 116
  | Water -> Graphics.rgb 152 200 240
  | Grass -> Graphics.rgb 112 200 160
  | Road -> Graphics.rgb 232 224 136
  | Gym -> Graphics.rgb 192 168 103
  | PokeCenter -> Graphics.rgb 216 104 96
  | House -> Graphics.rgb 200 168 240

(** [draw_map blocks] draws the map from a 2D array of blocks [blocks] *)
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

(** [draw_panel blocks] draws the bottom text panel of a screen under the map *)
let draw_panel blocks = 
  let panel_width = (Array.length blocks.(0)) * box_len in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 panel_width panel_height;
  Graphics.set_color panel_color;
  Graphics.set_line_width panel_outline;
  Graphics.draw_rect 0 0 panel_width panel_height

(** [draw_char p] draws the player on the map *)
let draw_char p = 
  let (x, y) = p.location in
  Graphics.set_color char_color;
  Graphics.fill_circle (x * box_len + box_len/2)
    (y * box_len + box_len/2 + panel_height) char_size;;

(** [graph_dims blocks] returns the dimensions of the graphical screen given a 
    map *)
let graph_dims blocks =
  let ncol = Array.length blocks.(0) in
  let nrow = Array.length blocks in
  " " ^ string_of_int (ncol * box_len) ^ "x" ^
  string_of_int (nrow * box_len + panel_height)

(** [display_text s] draws text [s] onto the bottom panel while player is 
    walking *)
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

(** [battle_panel_ht] is the height of the text panel during encounters and 
    battling *)
let battle_panel_ht = int_of_float (1.5 *. float_of_int panel_height)

(** [custom_outline color x1 y1 x2 y2 buffer size] draws an outline of [size] 
    thickness and [color] color with some [buffer] size within the given the 
    coordinates of a rectangle *)
let custom_outline color x1 y1 x2 y2 buffer size = 
  Graphics.set_color color;
  Graphics.set_line_width size;
  Graphics.draw_rect (x1 + buffer) (y1 + buffer) (x2 - 2 * buffer)
    (y2 - 2 * buffer)

(** [battle_bg_panel ()] draws the bottom left panel displayed during an 
    encounter *)
let bottom_bg_panel () = 
  let panel_width = size_x () in
  Graphics.set_color (Graphics.rgb 40 80 104);
  Graphics.fill_rect 0 0 panel_width battle_panel_ht;
  custom_outline Graphics.white 0 0 panel_width battle_panel_ht 13 5;
  custom_outline (Graphics.rgb 200 168 72) 0 0 panel_width battle_panel_ht 5 12;
  custom_outline Graphics.black 0 0 panel_width battle_panel_ht 0 5

(** [bottom_menu_panel ()] draws the bottom right menu panel displayed during
    an encounter. *)
let bottom_menu_panel () =
  let panel_width = size_x () in
  let new_x = panel_width / 2 in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect new_x 0 panel_width battle_panel_ht;
  custom_outline (Graphics.rgb 117 111 144) new_x 0 panel_width battle_panel_ht 
    5 12;
  custom_outline Graphics.black new_x 0 panel_width battle_panel_ht 0 5

(** [draw_bottom_panel ()] draws the bottom panel during an encounter. *)
let draw_bottom_panel () = 
  bottom_bg_panel ();
  bottom_menu_panel ()

(** [make_options ncol x y width height lst] makes the different options in 
    [lst] on a given menu panel *)
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

(** [list_of_stats pokemon] are the stats displayed of a Pokemon [pokemon] 
    during an encounter *)
let list_of_stats pokemon = 
  [(String.uppercase_ascii pokemon.name);
   "Lv" ^ string_of_int pokemon.stats.level]

(** [opt_lst ()] are the menu option buttons during an encounter *)
let opt_lst menu = make_options 2 (size_x () / 2) 0 (size_x () / 2) 
    battle_panel_ht (Array.to_list menu)

(** [draw_options] draws the list of option buttons *)
let rec draw_options = function
  | [] -> ()
  | opt :: t -> let (x, y) = opt.loc in
    Graphics.moveto x y;
    Graphics.set_color Graphics.black;
    Graphics.draw_string opt.text;
    draw_options t

(** [poke_panel x y poke] draws the Pokemon stats on the encounter screen *)
let poke_panel x y poke = 
  let width = size_x () / 3 in
  let height = size_y () / 5 in
  Graphics.set_color (Graphics.rgb 248 248 216);
  Graphics.fill_rect x y width height;
  Graphics.set_color (Graphics.rgb 58 82 52);
  Graphics.set_line_width 5;
  Graphics.draw_rect x y width height;
  let lst = list_of_stats poke in
  let txt = make_options 2 x y width height lst in
  draw_options txt

let render_menu (st : State.state) (est : State.menu_state) =
  let () = Graphics.open_graph (graph_dims st.map); in
  let () = Graphics.clear_graph () in 
  let () = draw_bottom_panel () in
  let () = poke_panel 50 275 est.opponent in
  let () = poke_panel 300 150 (List.hd est.player.poke_list) in
  let () = draw_options (opt_lst Menu.menu_lst) in
  let () = synchronize () in ()
