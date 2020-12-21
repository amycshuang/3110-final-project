open Player
open State
open Pokemon
open Gui
open Menu
open Walking
open Trainer

type gym_action =  StartBattle | EndBattle | Default 

let gym_key ch =
  match ch with 
  | 'b' -> StartBattle 
  | 'n' -> EndBattle 
  | _ -> Default 

(** [win_battle_money trainers] is the amount of pokecoins won after defeating
    a trainer. *)
let win_battle_money trainers = 1200 / (List.length trainers + 1)

(** [set_gym_start st t] is the state when starting a gym battle with a trainer
    [t]. *)
let set_gym_start st t =
  let mst =                 
    {status = Default;
     player = st.player; 
     opponent = t.poke_list; 
     hover = 0; 
     select = None;
     is_trainer = true;
     previous = None;
    } in 
  {st with status = Menu mst}

(** [give_player_money st] is the state after a player wins money from a trainer
    battle. *)
let give_player_money st = 
  let win_money = win_battle_money st.trainers in 
  let new_player = {st.player with balance = st.player.balance + win_money} in 
  {st with player = new_player}

(** [set_gym_win st] is the state after a gym battle. *)
let set_gym_win st = 
  let remaining_trainers = List.tl st.trainers in 
  let n_st = give_player_money st in 
  if List.length remaining_trainers = 0 then 
    let bag = 
      {n_st.player.bag with badge_case = ["Functional Programming Gym"]} in 
    let win_player = {n_st.player with bag = bag} in 
    {n_st with player = win_player; status = Win}
  else {n_st with trainers = remaining_trainers; status = WalkingGym} 

(** [process_gym input st] is the state during and after a gym battle. *)
let process_gym input st =
  match gym_key input, st.status with 
  | StartBattle, TrainerTalk t -> set_gym_start st t
  | EndBattle, TrainerOver -> set_gym_win st 
  | _, _ -> st