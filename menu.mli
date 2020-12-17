(** [bag_items bag] is the string representation of the items in the bag's
    inventory. *)
val bag_items: (Player.item * int) list -> string list

(** [process_encounter ch st] is the state after some key input during an 
    encounter. *)
val process_menu : char -> State.state -> State.menu_state -> State.state