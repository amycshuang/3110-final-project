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
  selected : bool;
}

type poke_option = {
  loc : int * int;
  poke : Pokemon.pokemon;
  selected : bool;
  first : bool
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


(** [draw_map st blocks] is the drawn map of [blocks]. *)
let draw_map st blocks = 
  let ncol = Array.length blocks.(0) in
  let nrow = Array.length blocks in
  for row = 0 to (nrow - 1) do
    for col = 0 to (ncol - 1) do
      Graphics.set_color (color_of_block (blocks.(row)).(col));
      Graphics.fill_rect (col * box_len)
        (row * box_len + panel_height) box_len box_len;
    done
  done

(** [draw_map st] draws the map depending on the status of state [st]. *)
let draw_map st =
  if st.status = Walking then draw_map st st.maps.(0)
  else draw_map st st.maps.(1)

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
    | "We have already battled!" as s -> display_text' s
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

let make_poke_options curr lst hover_choice =
  let x = (size_x ()) / 3 in
  (* let y = ((size_y ()) / 6) + 10 in *)
  let s_y = size_y () / 6 in
  let fst_loc = (10, (size_y () / 2)) in
  let fst_poke = List.hd lst in
  let fst_bool = if fst_poke = hover_choice then true else false in
  let fst_pokeopt = 
    {loc = fst_loc; poke = fst_poke; selected = fst_bool; first = true} in
  let rec making acc n = function
    | [] -> acc
    | h :: t -> let select_bool = if h = hover_choice then true else false in
      let new_loc = (x + 50, size_y () - (n * s_y) - 10) in
      let button = 
        {loc = new_loc; poke = h; selected = select_bool; first = false} in 
      making (button::acc) (n + 1) t in
  making [fst_pokeopt] 1 (List.tl lst)

(** [list_of_stats pokemon] are the stats displayed of a Pokemon [pokemon] 
    during an encounter *)
let list_of_stats pokemon = 
  [(String.uppercase_ascii pokemon.name);
   "Lv" ^ string_of_int pokemon.stats.level]

(** [opt_lst ()] are the menu option buttons during an encounter *)
let opt_lst menu hover = make_options 2 (size_x () / 2) 5 (size_x () / 2) 
    (battle_panel_ht - 15) (Array.to_list menu) menu.(hover)

(** [draw_options] draws the list of option buttons *)
let rec draw_options (opt_lst : option list) = 
  match opt_lst with
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
  let str = "TYPE/" ^ String.uppercase_ascii (string_from_type move.move_type) 
  in
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

let bag_panel () =
  let panel_width = size_x () in
  Graphics.set_color (Graphics.rgb 0 108 179);
  Graphics.fill_rect 0 0 panel_width battle_panel_ht;
  custom_outline (Graphics.rgb 0 73 101) 0 0 panel_width battle_panel_ht 0 5

let bag_yellowbg () =
  let big_ht = (size_y ()) - battle_panel_ht - 10 in
  let small_ht = big_ht / 3 in
  let small_width = ((size_x ()) - 15) / 3 in
  let big_width = small_width * 2 in
  Graphics.set_color (Graphics.rgb 244 194 110);
  Graphics.fill_rect 10 ((size_y ()) - small_ht - 5) small_width small_ht;
  Graphics.fill_rect (10 + small_width) (battle_panel_ht + 5) big_width big_ht;
  Graphics.set_color (Graphics.rgb 247 248 196);
  Graphics.fill_rect (20 + small_width) (battle_panel_ht + 15) (big_width - 20) 
    (big_ht - 20);
  Graphics.set_color (Graphics.rgb 219 125 71);
  Graphics.fill_rect 20 ((size_y ()) - (4 * small_ht / 5)) 
    (small_width - 20) 10;
  Graphics.set_color Graphics.black;
  Graphics.moveto ((small_width - 20) / 2) ((size_y ()) - 
                                            (4 * small_ht / 5) + 15);
  Graphics.draw_string "ITEMS"

let bag_lst bag hover = 
  let big_ht = (size_y ()) - battle_panel_ht - 10 in
  let small_width = ((size_x ()) - 15) / 3 in
  let big_width = small_width * 2 in
  let lst = str_bag_items bag in
  make_options 1 (20 + small_width) (battle_panel_ht + (((big_ht - 20)/ 2)) 
                                     + 15) (big_width / 3) ((big_ht - 20) / 3) 
    lst (Array.of_list lst).(hover)

(** [hp_bar' x y poke length stats str_clr] is a helper drawing function for 
    [hp_bar]. *)
let hp_bar' x y poke length stats str_clr = 
  if stats then
    let hp_string = (string_of_int poke.stats.hp) ^ "/ " ^ 
                    (string_of_int poke.stats.base_hp) in 
    let text_width = fst (Graphics.text_size hp_string) in
    let text_ht = snd (Graphics.text_size hp_string) in 
    Graphics.moveto (x + length - text_width / 2) (y - text_ht - 5);
    Graphics.set_color str_clr;
    Graphics.draw_string hp_string
  else ()

let hp_bar x y poke length stats str_clr = 
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
    (x + fst (Graphics.text_size "HP") + 5) y 
    (int_of_float (frac *. (float_of_int length))) 
    (snd (Graphics.text_size "HP"));
  hp_bar' x y poke length stats str_clr

let draw_poke x y poke size = 
  Graphics.set_color (color_of_poke poke.poke_type);
  Graphics.fill_circle x y size

let draw_fst_pokelst (poke : poke_option) = 
  let (x, y) = poke.loc in
  let width = (2 * (size_x ()) / 5) - 15 in
  let height = ((size_y ()) / 3) in
  Graphics.set_color (Graphics.rgb 91 201 226);
  Graphics.fill_rect x y width height;
  draw_poke (x + height / 5 + 5) (y + 3 * height / 4) poke.poke (height / 5);
  hp_bar (x + width / 5) (y + height / 5) poke.poke (3 * width / 5) true 
    Graphics.white;
  Graphics.set_color Graphics.white;
  let name = String.uppercase_ascii poke.poke.name in
  let name_ht = snd (Graphics.text_size name) in
  Graphics.moveto (x + 2 * width / 5) (y + 2 * height / 3);
  Graphics.draw_string (String.uppercase_ascii poke.poke.name);
  Graphics.moveto (x + 2 * width / 5 + 15) (y + 2 * height / 3 - name_ht - 2);
  Graphics.draw_string ("Lv" ^ (string_of_int poke.poke.stats.level));
  if poke.selected then begin
    Graphics.moveto (((x + 2 * width / 5)) - fst (Graphics.text_size ">") - 5) 
      (y + 2 * height / 3);
    Graphics.draw_string ">"
  end else ()

let draw_pokelst_rest (poke : poke_option) : unit =
  let (x, y) = poke.loc in
  let width = (size_x ()) - x - 15 in
  let height = ((size_y ()) / 6 - 10) in
  Graphics.set_color (Graphics.rgb 78 143 210);
  Graphics.fill_rect x y width height;
  draw_poke (x + (height / 3) + 5) (y + height / 2) poke.poke (height / 3);
  Graphics.moveto (x + width / 4) (y + height / 2);
  Graphics.set_color Graphics.white;
  let name = String.uppercase_ascii poke.poke.name in
  let name_ht = snd (Graphics.text_size name) in
  Graphics.draw_string name;
  Graphics.moveto (x + width / 4 + 15) (y + height / 2 - name_ht);
  Graphics.set_color Graphics.white;
  Graphics.draw_string ("Lv" ^ (string_of_int poke.poke.stats.level));
  hp_bar (x + width / 2 + 20) (y + height / 2) poke.poke (width / 3) true 
    Graphics.white;
  if poke.selected then begin
    Graphics.moveto ((x + width / 4) - fst (Graphics.text_size ">") - 5) 
      (y + height / 2);
    Graphics.draw_string ">"
  end else ()

let rec draw_poke_lst = function 
  | [] -> ()
  | h :: t -> if h.first then draw_fst_pokelst h else draw_pokelst_rest h;
    draw_poke_lst t

let pokelst_bg () = 
  Graphics.set_color (Graphics.rgb 92 158 154);
  Graphics.fill_rect 0 0 (size_x ()) (size_y ());
  custom_outline (Graphics.rgb 53 103 96) 0 0 (size_x () + 30) (size_y ()) 0 30;
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 5 5 (3 * (size_x ()) / 4) ((size_y ()) / 6);
  Graphics.set_color Graphics.black;
  Graphics.moveto 15 ((size_y ()) / 12);
  Graphics.draw_string "Choose a POKEMON."

(** [poke_panel x y poke] draws the Pokemon stats on the encounter screen *)
let poke_panel x y poke is_player = 
  let width = size_x () / 3 in
  let height = size_y () / 5 in
  Graphics.set_color (Graphics.rgb 248 248 216);
  Graphics.fill_rect x y width height;
  Graphics.set_color (Graphics.rgb 58 82 52);
  Graphics.set_line_width 5;
  Graphics.draw_rect x y width height;
  let () = hp_bar (x + 50) (y + (height / 4)) poke (width / 2) 
      is_player Graphics.black in
  let lst = list_of_stats poke in
  let txt = make_options 2 (x - 15) (y + 15) (4 * width / 3) height lst "" in
  draw_options txt

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
  Graphics.set_color (Graphics.rgb 238 238 238);
  Graphics.fill_rect 0 0 (size_x ()) (size_y ());
  let () = draw_bottom_panel (List.hd st.player.poke_list) in
  let () = poke_panel 50 275 (List.hd mst.opponent) false in
  let () = poke_panel 300 150 (List.hd mst.player.poke_list) true in
  let () = draw_poke (50 + (size_x () / 6)) 175 
      (List.hd st.player.poke_list) 50 in
  let () = draw_poke (300 + (size_x () / 6)) (160 + (size_y () / 3)) 
      (List.hd mst.opponent) 50 in
  let () = draw_options (opt_lst default_menu mst.hover) in
  let () = synchronize () in ()

let render_moves (st : State.state) (mst : State.menu_state) = 
  let () = Graphics.open_graph (graph_dims st.maps.(0)); in
  let () = Graphics.clear_graph () in 
  Graphics.set_color (Graphics.rgb 238 238 238);
  Graphics.fill_rect 0 0 (size_x ()) (size_y ());
  let () = draw_bottom_fight () in
  let () = poke_panel 50 275 (List.hd mst.opponent) false in
  let () = poke_panel 300 150 (List.hd mst.player.poke_list) true in
  let () = draw_poke (50 + (size_x () / 6)) 175 
      (List.hd st.player.poke_list) 50 in
  let () = draw_poke (300 + (size_x () / 6)) (160 + (size_y () / 3)) 
      (List.hd mst.opponent) 50 in
  let () = draw_options (mv_lst (List.hd st.player.poke_list) mst.hover) in
  let () = synchronize () in ()

let render_attack (st : State.state) (mst : State.menu_state) 
    poke1 poke2 player opp atk = 
  let () = Graphics.open_graph (graph_dims st.maps.(0)); in
  let () = Graphics.clear_graph () in
  Graphics.set_color (Graphics.rgb 238 238 238);
  Graphics.fill_rect 0 0 (size_x ()) (size_y ());
  let txt = attack_effectiveness poke1 poke2 atk in
  let () = draw_attack_info txt in
  let () = poke_panel 50 275 opp false in
  let () = poke_panel 300 150 player true in
  let () = draw_poke (50 + (size_x () / 6)) 175 (List.hd mst.player.poke_list)
      50 in
  let () = draw_poke (300 + (size_x () / 6)) (160 + (size_y () / 3)) 
      (List.hd mst.opponent) 50 in
  let () = synchronize () in ()

let render_bag (st : State.state) (mst : State.menu_state) =
  let () = Graphics.open_graph (graph_dims st.maps.(0)); in
  let () = Graphics.clear_graph () in
  let () = Graphics.set_color (Graphics.rgb 75 194 186) in
  let () = Graphics.fill_rect 0 0 (size_x ()) (size_y ()) in
  let () = bag_panel () in
  let () = bag_yellowbg () in
  let () = draw_options (bag_lst st.player.bag.inventory mst.hover) in
  let () = synchronize () in ()

let render_pokelst (st : State.state) (mst : State.menu_state) =
  let () = Graphics.open_graph (graph_dims st.maps.(0)); in
  let () = Graphics.clear_graph () in
  let () = pokelst_bg () in
  let pokelst = Array.of_list mst.player.poke_list in
  let selected = pokelst.(mst.hover) in
  let curr = List.hd mst.player.poke_list in
  let () = draw_poke_lst 
      (make_poke_options curr mst.player.poke_list selected) in
  let () = synchronize () in ()

let render_menu (st : State.state) (mst : State.menu_state) = 
  match mst.status with
  | Default | Heal | Catch -> render_default st mst
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
  | Bag -> render_bag st mst
  | PokeList -> render_pokelst st mst
  | _ -> failwith "unimplmented"

let center_header_color = Graphics.rgb 255 153 204 
let center_color = Graphics.rgb 102 178 255 
let hp_bad_color = Graphics.rgb 240 53 53
let hp_ok_color = Graphics.rgb 255 204 153
let hp_good_color = Graphics.rgb 129 210 153
let no_money_color = Graphics.rgb 255 153 153
let box_len_11 = box_len * 11

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

let render_pokecenter' x y title color = 
  Graphics.moveto x y; 
  Graphics.set_color color; 
  Graphics.draw_string title

let render_pokecenter (st: state) = 
  Graphics.clear_graph ();
  let () = render_pokecenter' 160 340 "Welcome to the Pokemon Center!" 
      center_header_color in 
  let () = 
    render_pokecenter' (box_len * 2) 300 "Your Pokemon" center_color in 
  let () = render_pokecenter_pkm (st.player.poke_list) (box_len * 2) 270 in 
  let () = render_pokecenter' box_len_11 300 "Pokecenter Options" 
      center_color in 
  let () = render_pokecenter_options pokecenter_options box_len_11 270  in
  let () = render_pokecenter' box_len_11 150 "Your Balance: " center_color in
  Graphics.moveto (box_len_11 + 85) 150;
  let () = if st.player.balance = 0 then Graphics.set_color no_money_color else
      Graphics.set_color Graphics.black in 
  Graphics.draw_string (string_of_int st.player.balance ^ " pokecoins");
  let () = render_pokecenter' box_len_11 120 "Your Bag: " center_color in
  let () = render_bag (Encounter.str_bag_items st.player.bag.inventory) 
      box_len_11 90 in 
  let () = if st.player.balance = 0 then render_no_money () else () in 
  let () = synchronize () in ()

let draw_people x y = 
  Graphics.set_color Graphics.black; 
  Graphics.fill_circle x y 50

let draw_pokeballs x y pokelst is_player length size =
  let space = length / 6 in
  let dx = if is_player then space else -1 * space in
  let rec draw_ball pkm x y n = 
    match (n < 6), pkm with 
    | true, [] -> Graphics.set_color Graphics.black;
      Graphics.set_line_width 1;
      Graphics.draw_circle x y size;
      draw_ball [] (x + dx) y (n + 1)
    | false, [] -> ()
    | _, p :: t ->
      Graphics.moveto x y;
      Graphics.set_color Graphics.black;
      Graphics.set_line_width 1;
      Graphics.draw_circle x y size; 
      if (p.stats.hp > 0 && n < 6) then begin 
        Graphics.set_color (Graphics.rgb 238 132 110);
        Graphics.fill_circle x y size
      end else ();
      draw_ball t (x + dx) y (n + 1) in
  draw_ball pokelst x y 0

let pokeball_bar x y pokelst is_player = 
  let length = size_x () / 2 - 20 in
  let ball_len = 3 * length / 5 in
  Graphics.set_color Graphics.black;
  Graphics.fill_rect x y length 5;
  let barx = if is_player then x + (length / 2) - (ball_len / 2)
    else x + (length / 2) + (ball_len / 2) in
  draw_pokeballs barx (y + 20) pokelst is_player ball_len 7

let draw_trainer_str strlst instructions = 
  let all_str = strlst @ [instructions] in
  let rec drawing x y = function
    | [] -> ()
    | h :: t -> begin
        Graphics.set_color Graphics.white;
        Graphics.moveto x y;
        Graphics.draw_string h;
        let str_ht = snd (Graphics.text_size h) in
        drawing x (y - str_ht - 5) t 
      end in
  drawing 20 (battle_panel_ht - 30) all_str

let draw_trainer_bg st strlst instructions = 
  Graphics.set_color (Graphics.rgb 197 255 187);
  Graphics.fill_rect 0 0 (size_x ()) (size_y ());
  let panel_width = size_x () in
  Graphics.set_color (Graphics.rgb 52 160 151);
  Graphics.fill_rect 0 0 panel_width battle_panel_ht;
  custom_outline (Graphics.rgb 119 74 77) 0 0 panel_width battle_panel_ht 3 7;
  custom_outline Graphics.black 0 0 panel_width battle_panel_ht 0 5;
  draw_trainer_str strlst instructions;
  draw_people (50 + (size_x () / 6)) 175;
  draw_people (300 + (size_x () / 6)) (160 + (size_y () / 3))  

let draw_win_poke x y width height lst =
  let s_x = width / 3 in 
  let s_y = height / 2 in
  let rec drawing n = function
    | [] -> ()
    | h :: t -> 
      let multx = n mod 3 in
      let multy = if n < 3 then 1 else 0 in
      draw_poke (multx * s_x + x) (multy * s_y + y) h 50; 
      drawing (n + 1) t in
  drawing 0 lst

let draw_win_bg () = 
  Graphics.set_color (Graphics.rgb 60 71 87);
  Graphics.fill_rect 0 0 (size_x ()) (size_y ());
  let panel_ht = (size_y ()) - 30 - battle_panel_ht in
  Graphics.set_color (Graphics.rgb 157 183 226);
  Graphics.fill_rect 0 battle_panel_ht (size_x ()) panel_ht;
  let txt = "YOU WON! Welcome to the HALL OF FAME!" in
  let (txtx, txty) = Graphics.text_size txt in
  Graphics.moveto (size_x () / 2 - txtx / 2) (battle_panel_ht / 2);
  Graphics.set_color Graphics.white;
  Graphics.draw_string txt

let render_trainertalk (trainer : Trainer.trainer) st = 
  let () = Graphics.open_graph (graph_dims st.maps.(0)); in
  let () = Graphics.clear_graph () in
  let instructions = "(Press 'b' to continue)" in
  let () = draw_trainer_bg st ((trainer.name ^ ": ") :: trainer.catchphrase) 
      instructions in
  let () = pokeball_bar 0 275 trainer.poke_list false in
  let () = pokeball_bar ((size_x ()) - (size_x () / 2) + 20) 150 
      st.player.poke_list true in
  let () = synchronize () in () 

let render_trainerover (trainer : Trainer.trainer) st = 
  let () = Graphics.open_graph (graph_dims st.maps.(0)); in
  let () = Graphics.clear_graph () in
  let instructions = "(Press 'n' to continue)" in
  let () = draw_trainer_bg st ((trainer.name ^ ": ") :: trainer.endphrase) 
      instructions in
  let () = synchronize () in () 

let render_win st = 
  let () = Graphics.open_graph (graph_dims st.maps.(0)); in
  let () = Graphics.clear_graph () in
  let () = draw_win_bg () in
  let poke_width = (5 * size_x () / 6) in
  let () = draw_win_poke (((size_x ()) / 2) - (poke_width / 2) + 50) 
      (battle_panel_ht + 55) poke_width ((size_y ()) - 20 - battle_panel_ht) 
      st.player.poke_list in
  let () = synchronize () in () 
