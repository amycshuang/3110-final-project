(** 
   Representation of the game GUI. 
*)

(** [render_walk st] renders the GUI for the player in state [st] when [st]'s
    status is Walking. *)
val render_walk : State.state -> unit

(** [render_menu st mst] renders the GUI for the player in state [st] and 
    menu_state [mst] when [st]'s status is Menu [mst]. *)
val render_menu : State.state -> State.menu_state -> unit

(** [render_pokecenter st] renders the GUI for the player in state [st]
    when [st]'s status is PokeCenter. *)
val render_pokecenter : State.state -> unit 

(** [render_no_money] renders a message for the player indicating that they 
    have no money. *)
val render_no_money : unit -> unit

(** [render_trainertalk trainer st] renders the GUI for the player in state [st]
    when [st]'s status is TrainerTalk. *)
val render_trainertalk : Trainer.trainer -> State.state -> unit

(** [render_trainerover st] renders the GUI for for the player in state [st]
    when [st]'s status is TrainerOver. *)
val render_trainerover : State.state -> unit

(** [render_win st] renders the GUI for the player in state [st]
    and status Win. *)
val render_win : State.state -> unit