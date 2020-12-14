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
let menu_lst = [|"FIGHT"; "BAG"; "POKEMON"; "RUN"|]

let hover_change (st : menu_state) dir =
  let new_hover = 
    match dir with
    | Up -> st.hover - 2
    | Left -> st.hover - 1
    | Down -> st.hover + 2
    | Right -> st.hover + 1
  in if new_hover < 4 && new_hover >= 0 then {st with hover = new_hover}
  else st

let menu_of_string = function
  | "FIGHT" -> Fight
  | "BAG" -> Bag
  | "POKEMON" -> PokeList
  | "RUN" -> Run
  | _ -> failwith "not an option"

let action_change mst = function
  | Move x -> hover_change mst x
  | Enter -> let select_menu = menu_of_string menu_lst.(mst.hover) in
    {mst with select = Some select_menu}

let select_change est = function
  | Some sel -> action_change est sel
  | None -> est

let menu_change (mst : menu_state) st menu =
  match menu with 
  | Fight -> let bst = 
               {player=mst.player;
                opponent=mst.opponent; 
                p_turn=true;
                hover=0;
                select=Some Fight} in {st with status = (Battle bst)}
  | Bag -> st
  | PokeList -> st
  | Run -> {st with status = Walking}

let rec process_menu input (st: state) (mst : State.menu_state) =
  let new_mst = select_change mst (encount_key input) in 
  match mst.select with
  | Some menu -> menu_change new_mst st menu
  | None -> {st with status = Menu new_mst}