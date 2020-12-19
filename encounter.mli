(** [str_bag_items b] is a string representation of the inventory in bag [b]. *)
val str_bag_items : (Player.item * int) list -> string list

(** [check_pokelist pkm_lst] is true if there are still pokemon able to battle
    in the pokelist, false otherwise. *)
val check_pokelist : Pokemon.pokemon list -> bool 

(** [process_fight mst st] is the state after selecting Fight in the menu. *)
val process_fight : State.menu_state -> State.state -> State.state

(** [process_bag mst st] is the state after selecting Bag in the menu. *)
val process_bag : State.menu_state -> State.state -> State.state

(** [process_pokelist mst st] is the state after selecting Pokelist in the 
    menu. *)
val process_pokelist : State.menu_state -> State.state -> State.state

(** [process_catch mst st] is the state after attempting to catch a pokemon. *)
val process_catch : State.menu_state -> State.state -> State.state

(** [process_heal mst st] is the state after attempting to heal the battling 
    pokemon with a potion. *)
val process_heal : State.menu_state -> State.state -> State.state

(** [process_switch mst st] is the state after attempting to switch the battling
    pokemon with another one in the pokelist. *)
val process_switch : State.menu_state -> State.state -> State.state

(** [process_attack mst st] is the state after the battling pokemon attack
    each other. *)
val process_attack : State.menu_state -> State.state -> State.state

(** [process_run st] is the state after selecting Run in the menu *)
val process_run : State.state -> State.state

(** [process_player_team st] is the state after the pokemon able to battle
    are moved to the front of a player's team. *)
val process_player_team : Pokemon.pokemon list -> State.state -> Player.player

(** [process_pokecenter st] is the state after rushing to the pokecenter 
    to heal a team of fainted pokemon. *)
val process_fainted : State.state -> State.state