(** [render st] renders the GUI for the player [p] walking on map [m] *)
val render_walk : State.state -> unit

val render_encounter : State.state -> State.encounter_state -> unit
