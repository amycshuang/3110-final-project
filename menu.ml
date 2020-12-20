open State
open Player
open Pokemon
open Command
open Walking
open Encounter

type direction = Up | Left | Right | Down 

(** The type of selection on the encounter screen *)
type selection = Move of direction| Enter | Back

let default_menu = [|"FIGHT"; "BAG"; "POKEMON"; "RUN"|]

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
    match st.status with
    | Default | Fight -> begin
        match dir with
        | Up -> st.hover - 2
        | Left -> st.hover - 1
        | Down -> st.hover + 2
        | Right -> st.hover + 1
      end
    | PokeList -> begin
        match st.hover, dir with
        | 0, Right -> st.hover + 1
        | 0, _ -> st.hover
        | 1, Up -> st.hover
        | _, Right -> st.hover
        | _, Left -> 0
        | _, Up -> st.hover - 1
        | _, Down -> st.hover + 1
      end
    | _ -> begin
        match dir with
        | Up -> st.hover - 1
        | Down -> st.hover + 1
        | _ -> st.hover
      end
  in 
  let max_len = 
    match st.status with
    | Default | Fight -> 4
    | PokeList -> List.length st.player.poke_list
    | Bag -> List.length st.player.bag.inventory
    | _ -> failwith "no hover needed" in
  if new_hover < max_len && new_hover >= 0 then 
    {st with hover = new_hover}
  else st

(** [is_switch pkm_lst pkm_name] is true if a pokemon with name [pkm_name] 
    is in the list [pkm_lst], false otherwise. *)
let is_switch pkm_lst pkm_name = 
  List.mem pkm_name 
    (List.map (fun p -> p.name ^ " Hp: " ^ string_of_int p.stats.hp) pkm_lst)

(** [menu_of_string mst] is the menu option associated with the string the 
    hover is on. *)
let menu_of_string (mst : menu_state) = 
  match mst.status with
  | Default -> begin
      match default_menu.(mst.hover) with 
      | "FIGHT" -> Fight
      | "BAG" -> Bag
      | "POKEMON" -> PokeList
      | "RUN" -> Run
      | _ -> failwith "not a default menu option"
    end
  | Fight -> begin
      let curr_pokelist = mst.player.poke_list in 
      let curr_pkm = List.hd curr_pokelist in 
      let p_attack = List.nth curr_pkm.move_set mst.hover in 
      let curr_opp_pokelist = mst.opponent in 
      let opp_pkm = List.hd curr_opp_pokelist in 
      let o_attack = opponent_move opp_pkm in 
      Attack {player_attack = p_attack; 
              opponent_attack = o_attack;
              battling_poke = [|curr_pkm; curr_pkm; opp_pkm; opp_pkm|]
             }
    end
  | Bag -> begin
      let bag = Array.of_list mst.player.bag.inventory in
      match fst (bag.(mst.hover)) with
      | Potion -> Heal
      | Pokeball -> Catch
    end
  | PokeList -> Switch
  | x -> x
(* | x -> begin 
   if (contains x "POKEBALL") then Catch
   else if (contains x "POTION") then Heal
   else if (is_switch mst.player.poke_list x) then Switch 
   else Attack 
   end *)

let action_change mst = function
  | Move x -> hover_change mst x
  | Enter -> let select_menu = menu_of_string mst in
    {mst with status = select_menu; select = Some select_menu}
  | Back -> {mst with status = Default; hover = 0}

let select_change est = function
  | Some sel -> action_change est sel
  | None -> est

let menu_change (mst : menu_state) st menu =
  match menu with 
  | Fight -> process_fight mst st 
  | Bag -> process_bag mst st 
  | PokeList -> process_pokelist mst st 
  | Catch -> process_catch mst st 
  | Heal -> process_heal mst st 
  | Switch -> process_switch mst st 
  | Attack atks -> process_attack {mst with status = Attack atks} st atks 
  | Run -> process_run mst st
  | _ -> failwith "idk yet"

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