open Player
open State
open Pokemon
open Gui

type center_action = Heal | Potion | Pokeball | Back | Default

(** The cost of a potion represented as an int *)
let potion_cost = 100 

(** The cost of a pokeball represented as an int *)
let pokeball_cost = 50

(** [pokecenter_key ch] maps the input key to an action *)
let pokecenter_key ch = 
  match ch with 
  | 'h' -> Heal
  | 'j' -> Potion 
  | 'k' -> Pokeball
  | 'b' -> Back
  | _ -> Default

let heal_hp (pkm : pokemon) = 
  let original_stats = pkm.stats in 
  let full_hp = original_stats.base_hp in 
  let healed_stats = {original_stats with hp = full_hp} in 
  {pkm with stats = healed_stats}

(** [purchase_potion player] is the player after a potion has been purchased. *)
let purchase_potion player = failwith "TODO"

(** [purchase_pokeball player] is the player after a pokeball has been 
    purchased. *)
let purchase_pokeball player = failwith "TODO"

(** [process_pokecenter input st] processes the state in the pokecenter. *)
let process_pokecenter input (st: State.state) =  
  let action = pokecenter_key input in 
  match action with 
  | Heal -> 
    let poke_list = st.player.poke_list in 
    let healed_poke = List.map heal_hp poke_list in 
    let new_player = {st.player with poke_list = healed_poke} in 
    {st with player = new_player}
  | Potion -> 
    let player_money = st.player.balance in 
    if player_money >= potion_cost then 
      let new_player = purchase_potion st.player in 
      {st with player = new_player}
    else 
      let _ = render_no_money in 
      st 
  | Pokeball -> 
    let player_money = st.player.balance in 
    if player_money >= pokeball_cost then 
      let new_player = purchase_pokeball st.player in 
      {st with player = new_player}
    else 
      let _ = render_no_money in 
      st 
  | Back -> {st with status = Walking}
  | Default -> st 