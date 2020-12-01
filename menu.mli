(** [encounter_menu] are the list of menu options during an encounter *)
val menu_lst : string array

(** [process_encounter ch st] is the state after some key input during an 
    encounter. *)
val process_menu : char -> State.state -> State.menu_state -> State.state