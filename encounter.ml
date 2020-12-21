open State
open Player
open Pokemon
open Walking
open Pokecenter

(** The exception raised when an invalid pokemon attack is selected. *)
exception InvalidAttack

(** [change_arr arr index] is array [arr] with element at [index] changed
    to 1. *)
let change_arr arr index = 
  arr.(index) <- 1;
  arr

(** [generate_catch_arr num_of_ones arr] is an array with [num_of_ones] 1's. 
    All non-one elements are 0's. *)
let rec generate_catch_arr num_of_ones arr = 
  if num_of_ones = 0 then arr 
  else 
    let index = Random.int 9 in 
    if arr.(index) = 1 then 
      generate_catch_arr num_of_ones arr 
    else 
      let new_arr = change_arr arr index in 
      generate_catch_arr (num_of_ones - 1) new_arr

(** [num_of_ones base_hp hp] is an integer based on [base_hp] and [hp]. *)
let num_of_ones base_hp hp =
  let base = float_of_int base_hp in
  let h = float_of_int hp in
  int_of_float (10. *. ((base -. h) /. base)) 

(** [throw_pokeball opp_pkm st] is the state after a pokeball is thrown
    at a wild pokemon. *)
let throw_pokeball opp_pkm st = 
  let num_of_ones = num_of_ones opp_pkm.stats.base_hp opp_pkm.stats.hp in 
  let catch_arr  = generate_catch_arr num_of_ones (Array.make 10 0) in 
  let catch_index = Random.int 9 in 
  let is_catch = catch_arr.(catch_index) in 
  if is_catch = 0 then st
  else 
    let new_player = catch_poke st.player opp_pkm in 
    {st with player = new_player; status = Walking}

(** [check_catch b opp_pkm st] is the state after attempting to catch a wild
    pokemon. *)
let check_catch (b : bag) opp_pkm (st : state) = 
  let ball_num = List.assoc Player.Pokeball b.inventory in 
  if ball_num > 0 && (not opp_pkm.caught) && List.length st.player.poke_list < 6
  then
    let dec_inv = List.hd b.inventory :: [Player.Pokeball, ball_num - 1]  in 
    let new_bag = {b with inventory = dec_inv} in 
    let new_player = {st.player with bag = new_bag} in 
    let new_st = {st with player = new_player} in 
    throw_pokeball opp_pkm new_st 
  else st 

(** [heal_poke st] is the state after a potion is used on a pokemon. *)
let heal_poke st =
  let poke_lst = st.player.poke_list in 
  let pkm = List.hd poke_lst in 
  let new_hp = pkm.stats.hp + 20 in 
  let new_poke_stats = 
    if new_hp > pkm.stats.base_hp then {pkm.stats with hp = pkm.stats.base_hp}  
    else {pkm.stats with hp = new_hp} in 
  let new_poke = {pkm with stats = new_poke_stats} in 
  let new_player =  
    {st.player with poke_list = new_poke :: (List.tl poke_lst)} in 
  {st with player = new_player}

(** [check_potion b st] is the state after attempting to heal a pokemon. *)
let check_potion (b : bag) (st : state) = 
  let potion_num = List.assoc Player.Potion b.inventory in
  if potion_num > 0 then
    let dec_inv = (Player.Potion, potion_num - 1) :: List.tl b.inventory in
    let new_bag = {b with inventory = dec_inv} in 
    let new_player = {st.player with bag = new_bag} in 
    let new_st = {st with player = new_player} in 
    heal_poke new_st 
  else st

let check_pokelist pkm_lst = 
  List.length (List.filter (fun p -> p.stats.hp > 0) pkm_lst) > 0 

(** [set_battle_team pkm_lst] is [pkm_lst] with the pokemon with hp greater
    than 0 first. *)
let set_battle_team pkm_lst = 
  let battle_able_pkm = List.filter (fun p -> p.stats.hp > 0) pkm_lst in 
  let fainted = List.filter (fun p -> p.stats.hp = 0) pkm_lst in 
  battle_able_pkm @ fainted

(** [get_move p_moveset move_name] is the move in [p_moveset] corresponding to 
    [move_name]. *)
let get_move p_moveset move_name = 
  match List.filter (fun p -> p.move_name = move_name) p_moveset with 
  | [] -> raise InvalidAttack
  | h :: t ->  h

(** [get_pokecenter_loc map] is the location of the pokecenter on [map]. *)
let get_pokecenter_loc map = 
  let ncol = Array.length map.(0) in 
  let nrow = Array.length map in 
  let loc = ref (0, 0) in 
  for row = 0 to (nrow - 1) do 
    for col = 0 to (ncol - 1) do 
      if map.(row).(col) = Block.PokeCenter then 
        loc := (col, row)
    done; 
  done;
  !loc

(** [str_poke_lst pkm_lst] is the string representation of the pokemon in 
    pokemon list [pkm_lst]. *)
let str_poke_lst pkm_lst = 
  Array.of_list 
    (List.map (fun p -> p.name ^ " Hp: " ^ string_of_int p.stats.hp) pkm_lst)

let rec str_bag_items = function 
  | [] -> []
  | (item, amt) :: t -> match item with 
    | Player.Potion ->  ("POTIONS: " ^ string_of_int amt) :: str_bag_items t 
    | Player.Pokeball ->("POKEBALLS: " ^ string_of_int amt) :: str_bag_items t 

let str_move_lst moves = 
  Array.of_list (List.map (fun m -> m.move_name) moves)

(** [opp_attack' st mst atks pokelist] is the state after the opponent pokemon
    attacks and the player's attacking pokemon faints.  *)
let opp_attack' st mst atks pokelist = 
  if check_pokelist pokelist then 
    let pokelist' = set_battle_team pokelist in 
    let battling_poke = List.hd pokelist' in 
    let player' = {st.player with poke_list = pokelist'} in 
    let mst' = {mst with player = player'; select = None} in 
    let () = atks.battling_poke.(1) <- battling_poke in 
    let mst'' = {mst' with status = Attack atks} in 
    {st with player = player'; status = Menu mst''}
  else       
    let loc = get_pokecenter_loc st.maps.(0) in 
    let player' = {st.player with location = loc; poke_list = pokelist} in 
    {st with player = player'; status = PokeCenter}

(** [opp_attack st mst atks] is the state after the opponent pokemon attacks. *)
let opp_attack st mst atks = 
  let poke_lst = st.player.poke_list in 
  let curr_pkm = List.hd poke_lst in 
  let opp_pkm = List.hd mst.opponent in 
  let curr_pkm' = battle_damage curr_pkm opp_pkm atks.opponent_attack in 
  let pkm_lst' = curr_pkm' :: List.tl poke_lst in 
  if curr_pkm'.stats.hp = 0 then 
    opp_attack' st mst atks pkm_lst'
  else 
    let player' = {st.player with poke_list = pkm_lst'} in 
    let mst' = {mst with player = player'} in 
    let () = atks.battling_poke.(1) <- List.hd pkm_lst' in 
    {st with player = player'; status = Menu mst'}

(** [update_player_poke curr_pkm opp_pkm pkm_lst st] is the player after
    the player's battling pokemon [curr_pkm] gains experience and levels up,
     if necessary. *)
let update_player_poke curr_pkm opp_pkm pkm_lst st = 
  let gain_exp_curr = increase_exp curr_pkm opp_pkm in 
  let level_up_curr = level_up gain_exp_curr in 
  let new_curr_pokelist = level_up_curr :: List.tl pkm_lst in 
  {st.player with poke_list = new_curr_pokelist}

(** [battle_over mst st] is the state when the battle is over, depending on 
    whether the battle was with a wild pokemon or a trainer. *)
let battle_over mst st = 
  if mst.is_trainer then 
    {st with status = TrainerOver} 
  else {st with status = Walking}


(** [player_attack atks st mst pokelist] is the state after the player 
    pokemon attacks and the opponent's current pokemon fainted.  *)
let player_attack' atks st mst pokelist curr_pkm opp_pkm opp_pkm' = 
  let player' = update_player_poke curr_pkm opp_pkm pokelist st in
  let st' = {st with player = player'} in 
  let opp_lst' = opp_pkm' :: List.tl mst.opponent in 
  if check_pokelist opp_lst' then 
    let opp_lst'' = set_battle_team opp_lst' in 
    let curr_opp' = List.hd opp_lst'' in 
    let mst' = 
      {mst with player = player'; opponent = opp_lst''; select = None} in 
    let () = atks.battling_poke.(2) <- curr_opp' in 
    let () = atks.battling_poke.(3) <- curr_opp' in 
    let atks' = {atks with opponent_attack = 
                             opponent_move curr_opp'} in 
    let mst'' = {mst' with status = Attack atks'} in 
    opp_attack {st' with status = Menu mst''} mst'' atks'
  else battle_over mst st


(** [player_attack curr_pkm opp_pkm pkm_lst st] is the state after a player's
    pokemon attacks the opponent pokemon [opp_pkm]. *)
let player_attack atks st mst = 
  let pokelist = st.player.poke_list in 
  let curr_pkm = List.hd pokelist in 
  let opp_pkm = List.hd mst.opponent in 
  let opp_pkm' = battle_damage opp_pkm curr_pkm atks.player_attack in 
  if opp_pkm'.stats.hp = 0 then 
    player_attack' atks st mst pokelist curr_pkm opp_pkm opp_pkm'
  else 
    let new_opp_lst = opp_pkm' :: List.tl mst.opponent in 
    let new_mst' = {mst with opponent = new_opp_lst; select = None} in 
    let () = atks.battling_poke.(3) <- List.hd new_opp_lst in 
    opp_attack {st with status = Menu new_mst'} new_mst' atks

let process_fight (mst : menu_state) st = 
  let new_mst = {mst with hover = 0; select = None} in 
  {st with status = Menu new_mst}

let process_bag (mst : menu_state) st =
  let new_mst = {mst with hover = 0; select = None} in 
  {st with status = Menu new_mst} 

let process_pokelist (mst : menu_state) st = 
  let new_mst = {mst with hover = 0; select = None} in 
  {st with status = Menu new_mst}

let process_catch (mst: menu_state) st = 
  let n_st = check_catch st.player.bag (List.hd mst.opponent) st in 
  if n_st.status = Walking then 
    {n_st with panel_txt = parse_pokelist n_st.player}
  else 
    let new_mst = {mst with status = Default; hover = 0; 
                            player = n_st.player; select = None} in 
    {n_st with status = Menu new_mst}

let process_heal (mst : menu_state) st = 
  let n_st = check_potion st.player.bag st in 
  let new_mst = {mst with status = Default; hover = 0; 
                          player = n_st.player; select = None} in 
  {n_st with status = Menu new_mst; panel_txt = (parse_bag n_st.player)}

let process_switch (mst: menu_state) st = 
  let pkm_arr = Array.of_list mst.player.poke_list in 
  if pkm_arr.(mst.hover).stats.hp > 0 then 
    let original_fst = pkm_arr.(0) in 
    let () = pkm_arr.(0) <- pkm_arr.(mst.hover) in 
    let () = pkm_arr.(mst.hover) <- original_fst in 
    let pkm_lst = Array.to_list pkm_arr in 
    let new_player = {st.player with poke_list = pkm_lst} in 
    let new_mst = {mst with status = Default; hover = 0; 
                            player = new_player; select = None} in 
    {st with player = new_player; status = Menu new_mst} 
  else st

let process_attack mst st atk = player_attack atk st mst

let process_run mst st =    
  if mst.is_trainer then 
    {st with status = WalkingGym}
  else {st with status = Walking}

let process_player_team pkm_lst st = 
  let hp_check_poke = set_battle_team pkm_lst in 
  {st.player with poke_list = hp_check_poke} 

let process_fainted st = 
  let loc = get_pokecenter_loc st.maps.(0) in 
  let new_player = {st.player with location = loc} in 
  {st with player = new_player}