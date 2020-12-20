(** 
   TODO: add comment
*)

(** [parse_bag p] parses the player's bag to display on the text panel *)
val parse_bag : Player.player -> string

(** [parse_pokelist p] parses the player's Pokemon list to display on the text 
    panel *)
val parse_pokelist : Player.player -> string

(** TODO - add doc *)
val trainer_on_block : State.state -> Trainer.trainer

(** [process_walk input st] is the state after some key input while the 
    player is walking *)
val process_walk : char -> State.state -> State.state