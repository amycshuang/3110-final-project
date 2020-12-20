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
  | Null -> Graphics.rgb 0 0 0
  | GymRoad -> Graphics.rgb 208 171 115
  | Exit -> Graphics.rgb 101 101 101
  | BrownGymFloor -> Graphics.rgb 236 222 187
  | GreyGymFloor -> Graphics.rgb 183 172 163
  | Trainer -> Graphics.rgb 97 95 216
  | ClarksonSpot -> Graphics.rgb 179 27 27

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

(** [draw_map st] draws the map depending on the status of state [st]. *)
let draw_map st =
  if st.status = Walking then 
    let blocks = st.maps.(0) in 
    let ncol = Array.length blocks.(0) in
    let nrow = Array.length blocks in
    for row = 0 to (nrow - 1) do
      for col = 0 to (ncol - 1) do
        Graphics.set_color (color_of_block (blocks.(row)).(col));
        Graphics.fill_rect (col * box_len)
          (row * box_len + panel_height) box_len box_len;
      done
    done
  else 
    let blocks = st.maps.(1) in 
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

let rec display_poke_text_bottom str x y = 
  match str with 
  | [] -> ()
  | [x] -> ()
  | h1 :: h2 :: t -> 
    Graphics.moveto x y; 
    Graphics.draw_string h1; 
    Graphics.draw_string (" " ^ h2);
    display_poke_text_bottom t (x + 100) y 

let rec display_poke_text_top str x y  acc = 
  match str with 
  | [] -> ()
  | [x] -> ()
  | h1 :: h2 :: t -> 
    Graphics.moveto x y; 
    Graphics.draw_string h1; 
    Graphics.draw_string (" " ^ h2);
    if acc = 2 then display_poke_text_bottom t 10 (y/2)
    else display_poke_text_top t (x + 100) y (acc + 1)

(** [display_text s] draws text [s] onto the bottom panel while player is 
    walking *)
let display_text' s =
  Graphics.moveto 10 50;
  Graphics.draw_string s

let render_walk (st : State.state) = 
  let () = Graphics.open_graph (graph_dims st.maps.(0)); in 
  let () = clear_graph () in
  let () = draw_map st in
  let () = draw_panel st.maps.(0) in
  let () = draw_char st.player in
  let () = 
    match st.panel_txt with 
    | "Use your WASD keys to move around the map" as s -> display_text' s 
    | "Invalid Key :( Use WASD keys to walk around, P - Pokelist, B - Bag" 
      as s -> display_text' s 
    | "You must battle in order!" as s -> display_text' s
    | _ -> 
      let str_lst = String.split_on_char ' ' st.panel_txt in 
      display_poke_text_top str_lst 10 50 0 in
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
let bottom_bg_panel poke = 
  let panel_width = size_x () in
  let txt = "What will " ^ String.uppercase_ascii poke.name ^ " do?" in
  Graphics.set_color (Graphics.rgb 40 80 104);
  Graphics.fill_rect 0 0 panel_width battle_panel_ht;
  custom_outline Graphics.white 0 0 panel_width battle_panel_ht 13 5;
  custom_outline (Graphics.rgb 200 168 72) 0 0 panel_width battle_panel_ht 5 12;
  custom_outline Graphics.black 0 0 panel_width battle_panel_ht 0 5;
  Graphics.set_color Graphics.white;
  Graphics.moveto 25 (battle_panel_ht - 40);
  Graphics.draw_string txt 

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
let draw_bottom_panel poke = 
  bottom_bg_panel poke;
  bottom_menu_panel ()

let bottom_moves_panel () =
  let panel_width = 2 * (size_x ()) / 3 in
  let new_x = 0 in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect new_x 0 panel_width battle_panel_ht;
  custom_outline (Graphics.rgb 117 111 144) new_x 0 panel_width battle_panel_ht 
    5 12;
  custom_outline Graphics.black new_x 0 panel_width battle_panel_ht 0 5

let bottom_info_panel () =
  let panel_width = size_x () / 3 in
  let new_x = size_x () - panel_width in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect new_x 0 panel_width battle_panel_ht;
  custom_outline (Graphics.rgb 117 111 144) new_x 0 panel_width battle_panel_ht 
    5 12;
  custom_outline Graphics.black new_x 0 panel_width battle_panel_ht 0 5

let draw_bottom_fight () =
  bottom_moves_panel ();
  bottom_info_panel ()

let draw_attack_info str =
  let panel_width = size_x () in
  Graphics.set_color (Graphics.rgb 40 80 104);
  Graphics.fill_rect 0 0 panel_width battle_panel_ht;
  custom_outline Graphics.white 0 0 panel_width battle_panel_ht 13 5;
  custom_outline (Graphics.rgb 200 168 72) 0 0 panel_width battle_panel_ht 5 12;
  custom_outline Graphics.black 0 0 panel_width battle_panel_ht 0 5;
  Graphics.set_color Graphics.white;
  Graphics.moveto 25 (battle_panel_ht - 40);
  Graphics.draw_string str

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
let opt_lst menu hover = make_options 2 (size_x () / 2) 5 (size_x () / 2) 
    (battle_panel_ht - 15) (Array.to_list menu) menu.(hover)

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

let show_move_info move =
  let str = "TYPE/" ^ String.uppercase_ascii (string_from_type move.move_type) in
  Graphics.set_color Graphics.black;
  let panel_width = size_x () / 3 in
  let new_x = size_x () - panel_width in
  Graphics.moveto (new_x + 25) 30;
  Graphics.draw_string str

let mv_lst poke hover = 
  let moves = poke.move_set in
  let move_str = str_move_lst moves in
  let panel_width = 2 * (size_x ()) / 3 in
  let () = show_move_info (Array.of_list moves).(hover) in 
  make_options 2 0 10 panel_width (battle_panel_ht - 15)
    (Array.to_list move_str) move_str.(hover)

let hp_bar x y poke length = 
  let tot_hp = float_of_int poke.stats.base_hp in
  let curr_hp = float_of_int poke.stats.hp in
  let frac = curr_hp /. tot_hp in
  let clr = if frac > (2. /. 3.) then Graphics.rgb 0 246 146
    else if frac > (1. /. 3.) then Graphics.rgb 247 236 126
    else Graphics.rgb 237 97 96 in
  Graphics.set_color (Graphics.rgb 58 82 52);
  Graphics.fill_rect 
    (x - 5) (y - 4) (fst (Graphics.text_size "HP") + 15 + length) 20;
  Graphics.set_color (Graphics.rgb 252 208 68);
  Graphics.moveto x y;
  Graphics.draw_string "HP";
  Graphics.set_color clr;
  Graphics.fill_rect 
    (x + fst (Graphics.text_size "HP") + 5) y (int_of_float (frac *. (float_of_int length))) (snd (Graphics.text_size "HP"))

(** [poke_panel x y poke] draws the Pokemon stats on the encounter screen *)
let poke_panel x y poke = 
  let width = size_x () / 3 in
  let height = size_y () / 5 in
  Graphics.set_color (Graphics.rgb 248 248 216);
  Graphics.fill_rect x y width height;
  Graphics.set_color (Graphics.rgb 58 82 52);
  Graphics.set_line_width 5;
  Graphics.draw_rect x y width height;
  let () = hp_bar (x + 50) (y + (height / 4)) poke (width / 2) in
  let lst = list_of_stats poke in
  let txt = make_options 2 (x - 15) (y + 15) (4 * width / 3) height lst "" in
  draw_options txt

let draw_poke x y poke = 
  Graphics.set_color (color_of_poke poke.poke_type);
  Graphics.fill_circle x y 50

let rec wait_time ref_time = 
  if Unix.gettimeofday () -. ref_time > 2.5 then 
    () 
  else begin
    Unix.sleepf(0.0001); 
    wait_time ref_time
  end

let render_default (st : State.state) (mst : State.menu_state) = 
  let () = Graphics.open_graph (graph_dims st.maps.(0)); in
  let () = Graphics.clear_graph () in 
  let () = draw_bottom_panel (List.hd st.player.poke_list) in
  let () = poke_panel 50 275 (List.hd mst.opponent) in
  let () = poke_panel 300 150 (List.hd mst.player.poke_list) in
  let () = draw_poke (50 + (size_x () / 6)) 175 (List.hd st.player.poke_list) in
  let () = draw_poke (300 + (size_x () / 6)) (160 + (size_y () / 3)) 
      (List.hd mst.opponent) in
  let () = draw_options (opt_lst default_menu mst.hover) in
  let () = synchronize () in ()

let render_moves (st : State.state) (mst : State.menu_state) = 
  let () = Graphics.open_graph (graph_dims st.maps.(0)); in
  let () = Graphics.clear_graph () in 
  let () = draw_bottom_fight () in
  let () = poke_panel 50 275 (List.hd mst.opponent) in
  let () = poke_panel 300 150 (List.hd mst.player.poke_list) in
  let () = draw_poke (50 + (size_x () / 6)) 175 (List.hd st.player.poke_list) in
  let () = draw_poke (300 + (size_x () / 6)) (160 + (size_y () / 3)) 
      (List.hd mst.opponent) in
  let () = draw_options (mv_lst (List.hd st.player.poke_list) mst.hover) in
  let () = synchronize () in ()

let render_attack (st : State.state) (mst : State.menu_state) poke1 poke2 player opp atk = 
  let () = Graphics.open_graph (graph_dims st.maps.(0)); in
  let () = Graphics.clear_graph () in
  let txt = attack_effectiveness poke1 poke2 atk in
  let () = draw_attack_info txt in
  let () = poke_panel 50 275 opp in
  let () = poke_panel 300 150 player in
  let () = draw_poke (50 + (size_x () / 6)) 175 (List.hd mst.player.poke_list) in
  let () = draw_poke (300 + (size_x () / 6)) (160 + (size_y () / 3)) 
      (List.hd mst.opponent) in
  let () = synchronize () in ()


(* let () = wait_time (Unix.gettimeofday ()) *)

let render_menu (st : State.state) (mst : State.menu_state) = 
  match mst.status with
  | Default -> render_default st mst
  | Fight -> render_moves st mst
  | Attack atks -> 
    let p_poke = atks.battling_poke.(0) in 
    let p_poke' = atks.battling_poke.(1) in 
    let o_poke = atks.battling_poke.(2) in 
    let o_poke' = atks.battling_poke.(3) in 
    render_attack st mst o_poke p_poke p_poke o_poke' atks.player_attack;
    wait_time (Unix.gettimeofday ());
    render_attack st mst p_poke o_poke p_poke' o_poke' atks.opponent_attack;
    wait_time (Unix.gettimeofday ())
  | Run -> render_walk st
  | _ -> failwith "unimplmented"


(************************* T E S T I N G *************************************)

(* let poke_lst = 
   Pokemon.poke_list_from_json (Yojson.Basic.from_file "starter_pokemon.json")

   let starter = List.hd poke_lst

   let test_opp = List.nth poke_lst 2

   let test_map = Block.json_to_map "map1.json"

   let test_st = init_state "test" starter test_map


   let test_mst : menu_state = {
   status = Default;
   player = test_st.player;
   opponent = [test_opp];
   hover = 0;
   select = None;
   p_turn = true;
   previous = None
   } *)

(* let () = render_moves test_st test_mst  *)

(*  let test_render () = render_menu test_st test_mst *)


let pokecenter_header_color = Graphics.rgb 255 153 204 
let pokecenter_color = Graphics.rgb 102 178 255 
let hp_bad_color = Graphics.rgb 240 53 53
let hp_ok_color = Graphics.rgb 255 204 153
let hp_good_color = Graphics.rgb 129 210 153
let no_money_color = Graphics.rgb 255 153 153

(** [hp_color hp base_hp] is the color of the drawn pokemon's hp. *)
let hp_color hp base_hp = 
  let float_hp = float_of_int hp in 
  let float_base = float_of_int base_hp in 
  if float_hp /. float_base >= 0.50 then hp_good_color
  else if float_hp /. float_base >= 0.2 then hp_ok_color
  else hp_bad_color

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
  "Press 'j' to buy a potion (100)";
  "Press 'k' to buy a pokeball (50)";
  "Press 'b' to leave the pokecenter"
]

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

let render_no_money () = 
  Graphics.moveto 150 30; 
  Graphics.set_color no_money_color; 
  Graphics.draw_string "You don't have enough money!";
  ()

let render_pokecenter (st: state) = 
  Graphics.clear_graph ();
  Graphics.moveto 160 340; 
  Graphics.set_color pokecenter_header_color; 
  Graphics.draw_string "Welcome to the Pokemon Center!";
  Graphics.moveto (box_len * 2) 300;
  Graphics.set_color pokecenter_color;
  Graphics.draw_string "Your Pokemon";
  let () = render_pokecenter_pkm (st.player.poke_list) (box_len * 2) 270 in 
  Graphics.moveto (box_len * 11) 300;
  Graphics.set_color pokecenter_color;
  Graphics.draw_string "Pokecenter Options";
  let () = render_pokecenter_options pokecenter_options(box_len * 11) 270  in
  Graphics.moveto (box_len * 11) 150; 
  Graphics.set_color pokecenter_color; 
  Graphics.draw_string "Your Balance: ";
  Graphics.moveto (box_len * 11 + 85) 150;
  let () = if st.player.balance = 0 then Graphics.set_color no_money_color else
      Graphics.set_color Graphics.black in 
  Graphics.draw_string (string_of_int st.player.balance ^ " pokecoins");
  Graphics.moveto (box_len * 11) 120;
  Graphics.set_color pokecenter_color; 
  Graphics.draw_string "Your Bag: ";
  let () = render_bag (Encounter.str_bag_items st.player.bag.inventory) (box_len * 11) 90 in 
  let () = 
    if st.player.balance = 0 then render_no_money () else () in 
  let () = synchronize () in ()


let draw_people x y p_color = 
  Graphics.set_color p_color; 
  Graphics.fill_circle x y 50

(* let render_battle_begin st = 
   let () = Graphics.open_graph (graph_dims st.maps.(0)); in
   let () = Graphics.clear_graph () in
   let () = draw_people (50 + (size_x () / 6)) 175 Graphics.black in 
   let () = draw_people (300 + (size_x () / 6)) (160 + (size_y () / 3)) 
      (Graphics.rgb 97 95 216) in 

   let () = synchronize () in () *)


(* (** [draw_panel blocks] draws the bottom text panel of a screen under the map *)
   let draw_battle_panel = 
   let panel_width = (Array.length blocks.(0)) * box_len in
   Graphics.set_color Graphics.white;
   Graphics.fill_rect 0 0 panel_width panel_height;
   Graphics.set_color panel_color;
   Graphics.set_line_width panel_outline;
   Graphics.draw_rect 0 0 panel_width panel_height

   let render_battle_end st = 
   let () = Graphics.open_graph (graph_dims st.maps.(0)); in
   let () = Graphics.clear_graph () in
   let () = draw_people (50 + (size_x () / 6)) 175 Graphics.black in 
   let () = draw_people (300 + (size_x () / 6)) (160 + (size_y () / 3)) 
      (Graphics.rgb 97 95 216) in 
   let () = synchronize () in () *)

let render_battle st = failwith "TODO"

(* let render_battle (st: state) = 
   match gst.status with 
   | CannotBattle -> () (** TODO *)
   | Begin -> render_battle_begin st gst 
   | Battling -> render_menu st gst.battle
   | Over -> () render_battle_end st gst *)
