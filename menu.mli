(** 
   Representation of a menu state.
*)

(** [default_menu] is the default menu options displayed when the Player first
    enters a battle.  *)
val default_menu : string array

(** [process_encounter ch st] is the state after some key input during an 
    encounter. *)
val process_menu : char -> State.state -> State.menu_state -> State.state