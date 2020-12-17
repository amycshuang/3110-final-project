open State
open Player
open Pokemon
open Command
open Str

type direction = Up | Left | Right | Down 

(** The type of selection on the encounter screen *)
type selection = Move of direction| Enter

(** [encount ch] is the correpsonding selection action to a key input. *)
let encount_key ch =
  match ch with
  | 'w' -> Some (Move Up)
  | 'a' -> Some (Move Left)
  | 's' -> Some (Move Down)
  | 'd' -> Some (Move Right)
  | 'f' -> Some Enter
  | _ -> None

let hover_change (st : menu_state) dir =
  let new_hover = 
    match dir with
    | Up -> st.hover - 2
    | Left -> st.hover - 1
    | Down -> st.hover + 2
    | Right -> st.hover + 1
  in 
  if new_hover < 4 && new_hover >= 0 then {st with hover = new_hover}
  else st

(* let contains s1 s2 =
   let regex = Str.regexp_string s2
   in try 
    let _ = Str.search_forward regex s1 0 in true 
   with 
    _ -> false *)

let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true
(* let is_pokeball x = 
   String.contains x 'P' && String.contains x 'O' && String.contains x 'K' && 
   String.contains x 'E *)

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
  (* let num_of_ones = num_of_ones opp_pkm.stats.base_hp opp_pkm.stats.hp in 
     let catch_arr  = generate_catch_arr num_of_ones (Array.make 10 0) in 
     let catch_index = Random.int 9 in 
     let is_catch = catch_arr.(catch_index) in 
     if is_catch = 0 then st
     else  *)
  let new_player = catch_poke st.player opp_pkm in 
  {st with player = new_player; status = Walking}

let check_catch (b : bag) opp_pkm (st : state) = 
  let ball_num = List.assoc Player.Pokeball b.inventory in 
  if ball_num > 0 && (not opp_pkm.caught) then
    let new_inv = List.remove_assoc Player.Pokeball b.inventory in 
    let dec_inv = (Player.Pokeball, ball_num - 1) :: new_inv in 
    let new_bag = {b with inventory = dec_inv} in 
    let new_player = {st.player with bag = new_bag} in 
    let new_st = {st with player = new_player} in 
    throw_pokeball opp_pkm new_st 
  else st 


let rec list_remove = function 
  | [] -> []  
  | h :: t -> t

let heal_poke pkm st =
  let poke_lst = list_remove st.player.poke_list in 
  let current_hp = pkm.stats.hp in 
  let new_hp = current_hp + 20 in 
  let new_poke_stats = 
    if new_hp > pkm.stats.base_hp then {pkm.stats with hp = pkm.stats.base_hp}  
    else {pkm.stats with hp = new_hp} in 
  let new_poke = {pkm with stats = new_poke_stats} in 
  let new_poke_lst = new_poke :: poke_lst in 
  let new_player =  {st.player with poke_list = new_poke_lst} in 
  {st with player = new_player}

let check_potion (b : bag) pkm (st : state) = 
  let potion_num = List.assoc Player.Potion b.inventory in
  if potion_num > 0 then
    let new_inv = List.remove_assoc Player.Potion b.inventory in
    let dec_inv = (Player.Potion, potion_num - 1) :: new_inv in
    let new_bag = {b with inventory = dec_inv} in 
    let new_player = {st.player with bag = new_bag} in 
    let new_st = {st with player = new_player} in 
    heal_poke pkm new_st 
  else st

let menu_of_string = function
  | "FIGHT" -> Fight
  | "BAG" -> Bag
  | "POKEMON" -> PokeList
  | "RUN" -> Run
  | x -> begin 
      if (contains x "POKEBALL") then Catch
      else if (contains x "POTION") then Heal
      else failwith "Unimplemented"
    end 

let action_change mst = function
  | Move x -> hover_change mst x
  | Enter -> let select_menu = menu_of_string mst.opt_lst.(mst.hover) in
    {mst with select = Some select_menu}

let select_change est = function
  | Some sel -> action_change est sel
  | None -> est

let rec bag_items = function 
  | [] -> []
  | (item, amt) :: t -> 
    if item = Player.Potion then 
      ("POTIONS: " ^ string_of_int amt) :: bag_items t 
    else 
      ("POKEBALLS: " ^ string_of_int amt) :: bag_items t 

let menu_change (mst : menu_state) st menu =
  match menu with 
  | Fight -> let bst = 
               {player=mst.player;
                opponent=mst.opponent; 
                p_turn=true;
                hover=0;
                select=Some Fight} in {st with status = (Battle bst)}
  | Bag -> let bag = st.player.bag.inventory in 
    let str_bag_inventory =  Array.of_list (bag_items bag) in 
    let new_mst = {mst with hover = 0; select = None; opt_lst = str_bag_inventory} in 
    {st with status = Menu new_mst} 
  | PokeList -> st 
  | Catch -> 
    check_catch st.player.bag (List.hd mst.opponent) st 
  | Heal -> check_potion st.player.bag (List.hd st.player.poke_list) st 
  | Run -> {st with status = Walking}

let rec process_menu input (st: state) (mst : State.menu_state) =
  let new_mst = select_change mst (encount_key input) in 
  match new_mst.select with
  | Some menu -> menu_change new_mst st menu
  | None -> {st with status = Menu new_mst}