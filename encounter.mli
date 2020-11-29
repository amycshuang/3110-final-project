(** [encounter_menu] are the list of menu options during an encounter *)
val encounter_menu : string array

(** [process_encounter ch st] is the state after some key input during an 
    encounter. *)
val process_encounter : char -> State.state -> State.encounter_state -> State.state