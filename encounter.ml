open State
open Player
open Pokemon
open Menu


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


(* let process_encounter st est = failwith "TODO"

   let switch pkm_name st e_st = 
   let pkm_lst = st.player.poke_list in 
   let selected_pkm = List.filter (fun p -> p.name = pkm_name) pkm_lst in 
   let unselected_pkm = List.filter (fun p -> p.name <> pkm_name) pkm_lst in 
   (List.hd selected_pkm) :: unselected_pkm 

   (** [run st] runs away from an encounter. *)
   let run st = {st with status = Walking}

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

   let throw_pokeball opp_pkm st e_st = 
   let num_of_ones = num_of_ones opp_pkm.stats.base_hp opp_pkm.stats.hp in 
   let catch_arr  = generate_catch_arr num_of_ones (Array.make 10 0) in 
   let catch_index = Random.int 9 in 
   let is_catch = catch_arr.(catch_index) in 
   if is_catch = 0 then st
   else 
    let new_player = catch_poke st.player opp_pkm in 
    {st with player = new_player; status = Walking}

   let check_catch (b : bag) opp_pkm (st : state) (e_st: encounter_state)= 
   let ball_num = List.assoc Player.Pokeball b.inventory in 
   if ball_num > 0 then 
    let new_inv = List.remove_assoc Player.Pokeball b.inventory in 
    let dec_inv = (Player.Pokeball, ball_num - 1) :: new_inv in 
    let new_bag = {b with inventory = dec_inv} in 
    let new_player = {st.player with bag = new_bag} in 
    let new_st = {st with player = new_player} in 
    throw_pokeball opp_pkm new_st e_st
   else st  *)

(** TODO: when encountering a pokemon if player tries to catch pokemon,
    check the pokemon's is_caught value. *)

(** TODO: function to heal the pokemon with a  *)
(* let heal_poke = failwith "TODO" *)