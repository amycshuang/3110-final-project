(** 
   Representation of a walking state.
*)

(** [parse_bag p] parses the player's bag to display on the text panel *)
val parse_bag : Player.player -> string

(** [parse_pokelist p] parses the player's Pokemon list to display on the text 
    panel *)
val parse_pokelist : Player.player -> string

(** [trainer_on_block st] is the trainer on a trainer block based on the
    player's location specified by the state [s]. *)
val trainer_on_block : State.state -> Trainer.trainer

(** [process_walk input st] is the state after some key input while the 
    player is walking *)
val process_walk : char -> State.state -> State.state