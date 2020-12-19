open State
open Player
open Pokemon
open Command
open Walking

type direction = Up | Left | Right | Down 

(** The type of selection on the encounter screen *)
type selection = Move of direction| Enter | Back

(** The exception raised when an invalid pokemon attack is selected. *)
exception InvalidAttack

(** The exception raised when the location of the PokeCenter is found. *)
exception PokeCenterLoc of int * int

(** [encount ch] is the correpsonding selection action to a key input. *)
let encount_key ch =
  match ch with
  | 'w' -> Some (Move Up)
  | 'a' -> Some (Move Left)
  | 's' -> Some (Move Down)
  | 'd' -> Some (Move Right)
  | 'f' -> Some Enter
  | 'b' -> Some Back
  | _ -> None

let hover_change (st : menu_state) dir =
  let new_hover = 
    match dir with
    | Up -> st.hover - 2
    | Left -> st.hover - 1
    | Down -> st.hover + 2
    | Right -> st.hover + 1
  in 
  if new_hover < Array.length st.opt_lst && new_hover >= 0 then 
    {st with hover = new_hover}
  else st

let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let change_arr arr index = 
  arr.(index) <- 1;
  arr

let rec generate_catch_arr num_of_ones arr = 
  if num_of_ones = 0 then arr 
  else 
    let index = Random.int 9 in 
    if arr.(index) = 1 then 
      generate_catch_arr num_of_ones arr 
    else 
      let new_arr = change_arr arr index in 
      generate_catch_arr (num_of_ones - 1) new_arr

let num_of_ones base_hp hp =
  let base = float_of_int base_hp in
  let h = float_of_int hp in
  int_of_float (10. *. ((base -. h) /. base)) 

let throw_pokeball opp_pkm st = 
  let num_of_ones = num_of_ones opp_pkm.stats.base_hp opp_pkm.stats.hp in 
  let catch_arr  = generate_catch_arr num_of_ones (Array.make 10 0) in 
  let catch_index = Random.int 9 in 
  let is_catch = catch_arr.(catch_index) in 
  if is_catch = 0 then st
  else 
    let new_player = catch_poke st.player opp_pkm in 
    {st with player = new_player; status = Walking}

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

let check_potion (b : bag) (st : state) = 
  let potion_num = List.assoc Player.Potion b.inventory in
  if potion_num > 0 then
    let dec_inv = (Player.Potion, potion_num - 1) :: List.tl b.inventory in
    let new_bag = {b with inventory = dec_inv} in 
    let new_player = {st.player with bag = new_bag} in 
    let new_st = {st with player = new_player} in 
    heal_poke new_st 
  else st

(** [check_pokelist pkm_lst] is true if there are still pokemon able to battle
    in the pokelist, false otherwise. *)
let check_pokelist pkm_lst = 
  List.length (List.filter (fun p -> p.stats.hp > 0) pkm_lst) > 0 

(** [set_battle_team pkm_lst] is the pkm_lst with pokemon with hp greater
    than 0 first. *)
let set_battle_team pkm_lst = 
  let battle_able_pkm = List.filter (fun p -> p.stats.hp > 0) pkm_lst in 
  let fainted = List.filter (fun p -> p.stats.hp = 0) pkm_lst in 
  battle_able_pkm @ fainted

let get_move p_moveset move_name = 
  match List.filter (fun p -> p.move_name = move_name) p_moveset with 
  | [] -> raise InvalidAttack
  | h :: t ->  h

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

let opp_attack st (mst : menu_state) = 
  let poke_lst = st.player.poke_list in 
  let curr_pkm = List.hd poke_lst in 
  let opp_pkm = List.hd mst.opponent in 
  let attack_move_2 = opponent_move opp_pkm in 
  let new_curr_pkm = battle_damage curr_pkm opp_pkm attack_move_2 in 
  let new_pkm_lst = new_curr_pkm :: List.tl poke_lst in 
  if new_curr_pkm.stats.hp = 0 then 
    if check_pokelist new_pkm_lst then 
      let new_curr_pkm_lst = set_battle_team new_pkm_lst in 
      let new_player = {st.player with poke_list = new_curr_pkm_lst} in 
      let new_mst = {mst with player = new_player} in 
      {st with player = new_player; status = Menu new_mst}
    else 
      let loc = get_pokecenter_loc st.map in 
      let new_player =  
        {st.player with location = loc; poke_list = new_pkm_lst} in 
      {st with player = new_player; status = State.PokeCenter}
  else       
    let new_player = {st.player with poke_list = new_pkm_lst} in 
    let new_mst = {mst with player = new_player} in 
    {st with player = new_player; status = Menu new_mst}

(** [update_player_poke curr_pkm opp_pkm pkm_lst st] is the player after
    the player's battling pokemon gains experience and levels up,
     if necessary. *)
let update_player_poke curr_pkm opp_pkm pkm_lst st = 
  let gain_exp_curr = increase_exp curr_pkm opp_pkm in 
  let level_up_curr = level_up gain_exp_curr in 
  let new_curr_pokelist = level_up_curr :: List.tl pkm_lst in 
  {st.player with poke_list = new_curr_pokelist}

let check_battle st mst =
  let curr_pokelist = st.player.poke_list in 
  let curr_pkm = List.hd curr_pokelist in 
  let attack_move_1 = get_move curr_pkm.move_set mst.opt_lst.(mst.hover) in 
  let opp_pkm = List.hd mst.opponent in 
  let new_opp = battle_damage opp_pkm curr_pkm attack_move_1 in 
  if new_opp.stats.hp = 0 then 
    let new_player = update_player_poke curr_pkm opp_pkm curr_pokelist st in 
    let new_st = {st with player = new_player} in 
    let new_opp_lst = new_opp :: List.tl mst.opponent in 
    let new_mst = {mst with player = new_player; opponent = new_opp_lst} in 
    if check_pokelist new_mst.opponent then 
      let new_opp_lst' = set_battle_team new_mst.opponent in 
      opp_attack new_st {new_mst with opponent = new_opp_lst'; select = None}
    else 
      {new_st with status = Walking}
  else 
    let new_opp_lst = new_opp :: (List.tl mst.opponent) in 
    opp_attack st {mst with opponent = new_opp_lst; select = None} 

let is_switch pkm_lst pkm_name = 
  List.mem pkm_name 
    (List.map (fun p -> p.name ^ " Hp: " ^ string_of_int p.stats.hp) pkm_lst)

let menu_of_string mst = match mst.opt_lst.(mst.hover) with 
  | "FIGHT" -> Fight
  | "BAG" -> Bag
  | "POKEMON" -> PokeList
  | "RUN" -> Run
  | x -> begin 
      if (contains x "POKEBALL") then Catch
      else if (contains x "POTION") then Heal
      else if (is_switch mst.player.poke_list x) then Switch 
      else Attack
    end 

let action_change mst = function
  | Move x -> hover_change mst x
  | Enter -> let select_menu = menu_of_string mst in
    {mst with select = Some select_menu}
  | Back -> {mst with opt_lst = [|"FIGHT"; "BAG"; "POKEMON"; "RUN"|]}

let select_change est = function
  | Some sel -> action_change est sel
  | None -> est

let rec bag_items = function 
  | [] -> []
  | (item, amt) :: t -> match item with 
    | Player.Potion ->  ("POTIONS: " ^ string_of_int amt) :: bag_items t 
    | Player.Pokeball -> ("POKEBALLS: " ^ string_of_int amt) :: bag_items t 
(* if item = Player.Potion then 
   ("POTIONS: " ^ string_of_int amt) :: bag_items t 
   else 
   ("POKEBALLS: " ^ string_of_int amt) :: bag_items t  *)

let str_poke_lst pkm_lst = 
  Array.of_list 
    (List.map (fun p -> p.name ^ " Hp: " ^ string_of_int p.stats.hp) pkm_lst)

let str_move_lst moves = 
  Array.of_list (List.map (fun m -> m.move_name) moves)

let menu_change (mst : menu_state) st menu =
  match menu with 
  | Fight -> 
    let str_moves = str_move_lst (List.hd (st.player.poke_list)).move_set in 
    let new_mst = 
      {mst with hover = 0; select = None; opt_lst = str_moves} in 
    {st with status = Menu new_mst}
  | Bag -> let bag = st.player.bag.inventory in 
    let str_bag_inventory =  Array.of_list (bag_items bag) in 
    let new_mst = 
      {mst with hover = 0; select = None; opt_lst = str_bag_inventory} in 
    {st with status = Menu new_mst} 
  | PokeList -> 
    let str_pkm = str_poke_lst st.player.poke_list in 
    let new_mst = 
      {mst with hover = 0; select = None; opt_lst = str_pkm} in 
    {st with status = Menu new_mst}
  | Catch -> 
    let n_st = check_catch st.player.bag (List.hd mst.opponent) st in 
    if n_st.status = Walking then 
      {n_st with panel_txt = parse_pokelist n_st.player}
    else 
      let str_bag_inventory = 
        Array.of_list (bag_items n_st.player.bag.inventory) in 
      let new_mst = 
        {mst with player = n_st.player; opt_lst = str_bag_inventory;
                  select = None} in 
      {n_st with status = Menu new_mst}
  | Heal ->  
    let n_st = check_potion st.player.bag st in 
    let str_bag_inventory = 
      Array.of_list (bag_items n_st.player.bag.inventory) in 
    let new_mst = 
      {mst with player = n_st.player; opt_lst = str_bag_inventory;
                select = None} in 
    {n_st with status = Menu new_mst; panel_txt = (parse_bag n_st.player)}
  | Switch -> 
    let pkm_arr = Array.of_list mst.player.poke_list in 
    if pkm_arr.(mst.hover).stats.hp > 0 then 
      let original_fst = pkm_arr.(0) in 
      let () = pkm_arr.(0) <- pkm_arr.(mst.hover) in 
      let () = pkm_arr.(mst.hover) <- original_fst in 
      let pkm_lst = Array.to_list pkm_arr in 
      let new_player = {st.player with poke_list = pkm_lst} in 
      let new_mst = {mst with player = new_player; 
                              opt_lst = str_poke_lst pkm_lst; select = None} in 
      {st with player = new_player; status = Menu new_mst} 
    else st
  | Attack -> check_battle st mst 
  | Run -> {st with status = Walking}

let rec process_menu input (st: state) (mst : State.menu_state) =
  let pkm_lst = st.player.poke_list in 
  if check_pokelist pkm_lst then 
    let hp_check_poke = set_battle_team pkm_lst in 
    let new_player = {st.player with poke_list = hp_check_poke} in 
    let new_st = {st with player = new_player} in 
    let new_mst = {mst with player = new_player} in 
    let new_mst' = select_change new_mst (encount_key input) in 
    match new_mst'.select with
    | Some menu -> menu_change new_mst' new_st menu
    | None -> {new_st with status = Menu new_mst'}
  else 
    let loc = get_pokecenter_loc st.map in 
    let new_player = {st.player with location = loc} in 
    {st with player = new_player; status = State.PokeCenter}
