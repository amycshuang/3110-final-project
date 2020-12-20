(** [play_game f] starts the adventure in file [f]. *)
val play_game : State.state -> unit

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit