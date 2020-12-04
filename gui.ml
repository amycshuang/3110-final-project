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
let color_of_block = function
  | TallGrass -> Graphics.rgb 88 165 116
  | Water -> Graphics.rgb 152 200 240
  | Grass -> Graphics.rgb 112 200 160
  | Road -> Graphics.rgb 232 224 136
  | Gym -> Graphics.rgb 192 168 103
  | PokeCenter -> Graphics.rgb 216 104 96
  | House -> Graphics.rgb 200 168 240

let color_of_poke = function
  | Bug -> Graphics.rgb 184 198 81
  | Fire -> Graphics.rgb 245 101 79
  | Water -> Graphics.rgb 74 170 255
  | Dragon -> Graphics.rgb 141 128 241
  | Electric -> Graphics.rgb 251 212 92
  | Flying -> Graphics.rgb 155 170 255
  | Ghost -> Graphics.rgb 127 127 198
  | Grass -> Graphics.rgb 140 212 114
  | Fairy -> Graphics.rgb 241 169 241
  | Fighting -> Graphics.rgb 198 114 101
  | Dark -> Graphics.rgb 141 114 101
  | Ground -> Graphics.rgb 226 198 115
  | Ice -> Graphics.rgb 125 212 255
  | Normal -> Graphics.rgb 183 183 169
  | Poison -> Graphics.rgb 184 114 169
  | Psychic -> Graphics.rgb 247 114 169
  | Rock -> Graphics.rgb 197 184 127
  | Steel -> Graphics.rgb 181 181 195

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
let make_options ncol x y width height lst hover_str =
  let s_x = width / ncol in 
  let s_y = height / ((List.length lst) / ncol ) in
  let rec making acc = function
    | [] -> acc
    | h :: t -> let n = (List.length t) + 1 in
      let multx = n mod ncol in
      let multy = (n + 1) / ncol - 1 in
      let select_bool = if h = hover_str then true else false in
      let button = {loc=((multx * s_x + (s_x / 3) + x),
                         ((multy * s_y) + (s_y /3 + y))); text=h; 
                    selected=select_bool} in making (button::acc) t in
  making [] lst

(** [list_of_stats pokemon] are the stats displayed of a Pokemon [pokemon] 
    during an encounter *)
let list_of_stats pokemon = 
  [(String.uppercase_ascii pokemon.name);
   "Lv" ^ string_of_int pokemon.stats.level]

(** [opt_lst ()] are the menu option buttons during an encounter *)
let opt_lst menu hover = make_options 2 (size_x () / 2) 0 (size_x () / 2) 
    battle_panel_ht (Array.to_list menu) menu.(hover)

(** [draw_options] draws the list of option buttons *)
let rec draw_options = function
  | [] -> ()
  | opt :: t -> let (x, y) = opt.loc in
    Graphics.moveto x y;
    Graphics.set_color Graphics.black;
    Graphics.draw_string opt.text;
    if opt.selected then begin
      Graphics.moveto (x - 5 - fst (Graphics.text_size ">")) y;
      Graphics.draw_string ">";
    end
    else ();
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
  let txt = make_options 2 x y width height lst "" in
  draw_options txt

let draw_poke x y poke = 
  Graphics.set_color (color_of_poke poke.poke_type);
  Graphics.fill_circle x y 50

let render_menu (st : State.state) (mst : State.menu_state) = ()
(* let () = Graphics.open_graph (graph_dims st.map); in
   let () = Graphics.clear_graph () in 
   let () = draw_bottom_panel () in
   let () = poke_panel 50 275 (List.hd (State.get_opponent est.opponent)) in
   let () = poke_panel 300 150 (List.hd est.player.poke_list) in
   let () = draw_poke (50 + (size_x () / 6)) 175 (List.hd st.player.poke_list) in
   let () = draw_poke (300 + (size_x () / 6)) (160 + (size_y () / 3)) 
    (List.hd (State.get_opponent est.opponent)) in
   let () = draw_options (opt_lst Menu.menu_lst mst.hover) in
   let () = synchronize () in () *)


(************************* T E S T I N G *************************************)

let poke_lst = 
  Pokemon.poke_list_from_json (Yojson.Basic.from_file "starter_pokemon.json")

let starter = List.hd poke_lst

let test_opp = List.nth poke_lst 2

let test_map = Block.json_to_map "map1.json"

let test_st = init_state "test" starter test_map

(* let test_mst : menu_state = {
   player = test_st.player;
   opponent = test_opp;
   hover = 0;
   select = None
   } *)

(* let test_render () = render_menu test_st test_mst *)

(** [hp_color hp base_hp] is the color of the drawn pokemon's hp. *)
let hp_color hp base_hp = 
  let float_hp = float_of_int hp in 
  let float_base = float_of_int base_hp in 
  if float_hp /. float_base >= 0.50 then Graphics.green 
  else if float_hp /. float_base >= 0.2 then Graphics.yellow 
  else Graphics.red

let rec render_pokecenter_pkm pkm_lst s_x s_y = 
  match pkm_lst with 
  | [] -> ()
  | pkm :: t -> 
    Graphics.moveto s_x s_y; 
    Graphics.set_color Graphics.black; 
    Graphics.draw_string pkm.name; 
    Graphics.moveto (s_x + 80) s_y;
    Graphics.set_color (hp_color pkm.stats.hp pkm.stats.base_hp);
    Graphics.draw_string ("hp: " ^ string_of_int pkm.stats.hp 
                          ^ " / " ^ string_of_int pkm.stats.base_hp);
    render_pokecenter_pkm t s_x (s_y - 30)

let pokecenter_options = [
  "Press 'h' to heal your pokemon";
  "Press 'j' to buy a potion";
  "Press 'k' to buy a pokeball";
  "Press 'b' to leave the pokecenter"
]

(** [bag_items bag] is the string representation of the items in the bag's
    inventory. *)
let rec bag_items = function 
  | [] -> []
  | (item, amt) :: t -> 
    if item = Player.Potion then 
      ("Potions: " ^ string_of_int amt) :: bag_items t 
    else 
      ("Pokeballs: " ^ string_of_int amt) :: bag_items t 

let rec render_pokecenter_options options s_x s_y = 
  match options with 
  | [] -> ()
  | opt :: t -> 
    Graphics.moveto s_x s_y; 
    Graphics.set_color Graphics.black; 
    Graphics.draw_string opt; 
    render_pokecenter_options t s_x (s_y - 30)

let rec render_bag bag_items s_x s_y = 
  match bag_items with 
  | [] -> () 
  | b :: t -> 
    Graphics.moveto s_x s_y; 
    Graphics.set_color Graphics.black; 
    Graphics.draw_string b; 
    render_bag t s_x (s_y - 30)

let render_no_money = 
  Graphics.moveto 200 60; 
  Graphics.set_color Graphics.black; 
  Graphics.draw_string "You don't have enough money!";
  let () = synchronize () in ()

let render_pokecenter (st: state) = 
  Graphics.clear_graph ();
  Graphics.moveto 160 340; 
  Graphics.set_color Graphics.magenta; 
  Graphics.draw_string "Welcome to the Pokemon Center!";
  Graphics.moveto (box_len * 2) 300;
  Graphics.set_color Graphics.cyan;
  Graphics.draw_string "Your Pokemon";
  let () = render_pokecenter_pkm (st.player.poke_list) (box_len * 2) 270 in 
  Graphics.moveto (box_len * 11) 300;
  Graphics.set_color Graphics.cyan;
  Graphics.draw_string "Pokecenter Options";
  let () = render_pokecenter_options pokecenter_options(box_len * 11) 270  in
  Graphics.moveto (box_len * 11) 150; 
  Graphics.set_color Graphics.cyan; 
  Graphics.draw_string "Your Balance: ";
  Graphics.moveto (box_len * 11 + 85) 150;
  Graphics.set_color Graphics.black; 
  Graphics.draw_string (string_of_int st.player.balance);
  Graphics.moveto (box_len * 11) 120;
  Graphics.set_color Graphics.cyan; 
  Graphics.draw_string "Your Bag";
  let () = render_bag (bag_items st.player.bag.inventory) (box_len * 11) 90 in 
  let () = if st.player.balance < 50 then 
      render_no_money else () in 
  let () = synchronize () in ()