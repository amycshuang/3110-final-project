open State
open Player
open Pokemon
open Command
open Walking
open Encounter

type direction = Up | Left | Right | Down 

(** The type of selection on the encounter screen *)
type selection = Move of direction| Enter | Back

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

(** [contains s1 s2] is true if string [s1] contains string [s2]. *)
let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

(** [is_switch pkm_lst pkm_name] is true if a pokemon with name [pkm_name] 
    is in the list [pkm_lst], false otherwise. *)
let is_switch pkm_lst pkm_name = 
  List.mem pkm_name 
    (List.map (fun p -> p.name ^ " Hp: " ^ string_of_int p.stats.hp) pkm_lst)

(** [menu_of_string mst] is the menu option associated with the string the 
    hover is on. *)
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

let menu_change (mst : menu_state) st menu =
  match menu with 
  | Fight -> process_fight mst st 
  | Bag -> process_bag mst st 
  | PokeList -> process_pokelist mst st 
  | Catch -> process_catch mst st 
  | Heal ->  process_heal mst st 
  | Switch -> process_switch mst st 
  | Attack -> process_attack mst st
  | Run -> process_run st

let rec process_menu input (st: state) (mst : State.menu_state) =
  let pkm_lst = st.player.poke_list in 
  if check_pokelist pkm_lst then 
    let player = process_player_team pkm_lst st in 
    let new_st = {st with player = player} in 
    let new_mst = {mst with player = player} in  
    let new_mst' = select_change new_mst (encount_key input) in 
    match new_mst'.select with
    | Some menu -> menu_change new_mst' new_st menu
    | None -> {new_st with status = Menu new_mst'}
  else process_fainted st 

