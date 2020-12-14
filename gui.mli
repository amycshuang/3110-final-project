(** [render_walk st] renders the GUI for the player in state [st] *)
val render_walk : State.state -> unit

(** [render_encounter st e_st] renders the GUI for the player in state [st] and 
    encounter state [e_st] *)
val render_menu : State.state -> State.menu_state -> unit

(** [render_pokecenter pst] renders the GUI for the player in state [st]. *)
val render_pokecenter : State.state -> unit 

(** [render_no_money] renders a message for the player indicating they have no
    money. *)
val render_no_money : unit -> unit