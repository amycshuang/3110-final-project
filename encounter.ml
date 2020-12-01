open State
open Player
open Pokemon
open Command

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

(** [encounter_menu] are the list of menu options during an encounter *)
let encounter_menu = [|"FIGHT"; "BAG"; "POKEMON"; "RUN"|]

let move_select est dir =
  let new_hover = 
    match dir with
    | Up -> est.hover - 2
    | Left -> est.hover - 1
    | Down -> est.hover + 2
    | Right -> est.hover + 1
  in if new_hover < 4 && new_hover >= 0 then {est with hover = new_hover}
  else est

let menu_of_string = function
  | "FIGHT" -> Fight
  | "BAG" -> Bag
  | "POKEMON" -> PokeList
  | "RUN" -> Run
  | _ -> failwith "not an option"

let select_action est = function
  | Move x -> move_select est x
  | Enter -> let select_menu = menu_of_string encounter_menu.(est.hover) in
    {est with select = Some select_menu}

let select_change est = function
  | Some sel -> select_action est sel
  | None -> est

(** this is all fake, just for testing, delete when done. *)
let choose_message = "Choose what you want to do"
let no_pokeball_msg = "You do not have any pokeballs!"

let process_menu (est : encounter_state) st menu =
  match menu with 
  | Fight -> let bst = {player=est.player; opponent=est.opponent; p_turn=false} in {st with status = (Battling bst)}
  | Bag -> st
  | PokeList -> st
  | Run -> {st with status = Walking}

(* TODO : process encounter*)
let rec process_encounter input (st: state) (est : State.encounter_state) =
  let new_est = select_change est (encount_key input) in 
  match est.select with
  | Some menu -> process_menu new_est st menu
  | None -> {st with status = Encounter new_est}

(* match e_st.select *)
(* let player = e_st.player in 
   let opp_pkm = e_st.opponent in 
   let original_hp = opp_pkm.stats.hp in 
   ANSITerminal.(print_string [cyan] choose_message);
   print_endline "";
   match fake_parse (read_line ()) parse_encounter with 
   | Pokeball -> let new_player = catch_poke player opp_pkm in
   {st with player=new_player}
   | _ ->  failwith "unimplemented" *)
(* | Potion -> 
   | Run -> 
   | Attack name ->  
   | Switch pkm ->  *)
(** do something *)
(* let newSt = something in 
   play_game newSt
   () *)

(* let select_opt st est = function
   | Fight ->
   | PokeList ->
   | Bag -> 
   | Run -> play_game {st with status=Walking}
*)

let switch pkm_name st e_st = 
  let pkm_lst = st.player.poke_list in 
  let selected_pkm = List.filter (fun p -> p.name = pkm_name) pkm_lst in 
  let unselected_pkm = List.filter (fun p -> p.name <> pkm_name) pkm_lst in 
  (List.hd selected_pkm) :: unselected_pkm

(** [run st] runs away from an encounter. *)
(* let run st = 
   let new_st = {st with status = Walking} in 
   play_game new_st *)

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

(* let throw_pokeball opp_pkm st e_st = 
   let num_of_ones = num_of_ones opp_pkm.stats.base_hp opp_pkm.stats.hp in 
   let catch_arr  = generate_catch_arr num_of_ones (Array.make 10 0) in 
   let catch_index = Random.int 9 in 
   let is_catch = catch_arr.(catch_index) in 
   if is_catch = 0 then process_encounter st e_st
   else 
    let new_player = catch_poke st.player opp_pkm in 
    let new_st = {st with player = new_player; status = Walking} in
    play_game new_st *)

(* let check_catch (b : bag) opp_pkm (st : state) (e_st: encounter_state)= 
   let ball_num = List.assoc Player.Pokeball b.inventory in 
   if ball_num > 0 then 
    let new_inv = List.remove_assoc Player.Pokeball b.inventory in 
    let dec_inv = (Player.Pokeball, ball_num - 1) :: new_inv in 
    let new_bag = {b with inventory = dec_inv} in 
    let new_player = {st.player with bag = new_bag} in 
    let new_st = {st with player = new_player} in 
    throw_pokeball opp_pkm new_st e_st
   else 
    process_encounter st e_st *)
