open Player
open State
open Pokemon
open Gui

type center_action = Heal | BPotion | BPokeball | Back | Default

(** The cost of a potion represented as an int *)
let potion_cost = 100 

(** The cost of a pokeball represented as an int *)
let pokeball_cost = 50

(** [pokecenter_key ch] maps the input key to an action *)
let pokecenter_key ch = 
  match ch with 
  | 'h' -> Heal
  | 'j' -> BPotion 
  | 'k' -> BPokeball
  | 'b' -> Back
  | _ -> Default

(** [heal_hp pkm] restores the hp of a pokemon [pkm] to its base stat. *)
let heal_hp (pkm : pokemon) = 
  let original_stats = pkm.stats in 
  let full_hp = original_stats.base_hp in 
  let healed_stats = {original_stats with hp = full_hp} in 
  {pkm with stats = healed_stats}

(** [purchase player money item] is the player after an item [item] has been 
    purchased.  *)
let purchase player money item = 
  let curr_inventory = player.bag.inventory in 
  let potion_num = List.assoc item curr_inventory in 
  let old_inv = List.remove_assoc item curr_inventory in 
  let new_inv = (item, potion_num + 1) :: old_inv in 
  let new_balance = money - potion_cost in 
  let new_bag = {player.bag with inventory = new_inv} in 
  {player with bag = new_bag; balance = new_balance}

(** [process_pokecenter input st] processes the state in the pokecenter. *)
let process_pokecenter input (st: State.state) =  
  let action = pokecenter_key input in 
  match action with 
  | Heal -> 
    let poke_list = st.player.poke_list in 
    let healed_poke = List.map heal_hp poke_list in 
    let new_player = {st.player with poke_list = healed_poke} in 
    {st with player = new_player}
  | BPotion -> 
    let player_money = st.player.balance in 
    if player_money >= potion_cost then 
      let new_player = purchase st.player player_money Player.Potion in 
      {st with player = new_player}
    else 
      st 
  | BPokeball -> 
    let player_money = st.player.balance in 
    if player_money >= pokeball_cost then 
      let new_player = purchase st.player player_money Player.Pokeball in 
      {st with player = new_player}
    else 
      st 
  | Back -> {st with status = Walking}
  | Default -> st 