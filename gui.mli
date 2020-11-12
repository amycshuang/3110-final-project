(** [render st] renders the GUI for the player [p] walking on map [m] *)
val render : State.state -> unit

val render_encounter : State.state -> State.block -> Pokemon.t -> Pokemon.t -> unit
