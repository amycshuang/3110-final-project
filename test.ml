(**
   TEST PLAN:

   A) OUNIT TESTS:

   - [pokemon.ml]
     Test cases were created for all public functions of [pokemon.ml] using
     glass box testing. We passed in pokemon and checked if the functions 
     correctly returned a pokemon with the correct stats (changed by
     leveling up, battle damage, increasing exp, etc.). We also checked for valid
     types and starter pokemon.

    This demonstrates correctness because all public functions of [pokemon.ml]
    are tested with different types of pokemon and moves and different stats 
    levels, which show that the mechanics for updating pokemon and checking for
    a pokemon's validity are working correclty.

   - [command.ml]
     Test cases were created on all public functions of [command.ml] using glass
     box testing. Phrases were passed as inputs into each function and then
     checked to see if they were correctly parsed into commands. We performed
     tests for all possible maps, starter pokemon, and possible phrases inputted
     by a user, including invalid inputs.

    This shows correctness as all possible types of inputs for each function
    as well as the various exceptions that they could raise were tested for to
    ensure that phrases were parsed.

   - [block.ml]
     Test cases were created for all public functions of [block.ml] using glass
     box testing. We passed in testing jsons, potential block types, as well
     as test matrices to check for the correctness of our functions. We also
     tested for invalid block_types passed in. 


    Two functions, [poke_rand] and [spawn_poke], were manually tested using our
    GUI to check if a random pokemon spawned on a tall grass block and 
    water block as the player walked onto a block. We also checked for the 
    correctness of the types, as only water pokemon should spawn on the water 
    blocks.

    This demonstrates correctness as all public functions in [block.ml] are
    tested on equality for expected outputs using a varied set of inputs, 
    including those that raise exceptions.

   - [trainer.ml]
     Test cases were created for [trainer.ml] by using glass box testing. Our
     approach to creating these tests were to check if a trainer json had been
     parsed correctly into type trainer. We passed in a JSON with a list of 
     trainers and checked for the correctness of their names and catchphrases.

   This demonstrates correctness as the functions in [trainer.ml] are tested
   on equality for expected outputs parsed from trainers.json. Using varied
   inputs and different trainers, we ensured that the parsing function in
   [trainer.ml] created trainers correctly. 

   B) MANUAL TESTS:

    For our manual tests, they are split into two types: those that involve the
    GUI and those that involve JSON building.

    GUI Testing:
    Due to our game being mainly graphics based and playable through XQuartz,
    many of our modules that involve changing the game state were manually
    tested through rendering on the GUI to check if the desired screens
    were drawn. By running make play, this initializes the game and displays
    the GUI that continues to update as the user interacts with the game.

   - [gui.ml] and [main.ml] were tested manually by using valid and invalid 
     keystrokes used to interact with the GUI itself. The main keys used
     to interact with the GUI are 'w', 'a', 's', 'd', 'f', and 'b', with the WASD
     as the keys used to move the player and pointers, 'f' as the selection key,
     and 'b' as a back key. Invalid keystrokes were also tested using [gui.ml]
     by displaying an "Invalid Key" message on the GUI itself.

    [main.ml] handles checking the status of particular state and rendering the
    correct GUI screen from [gui.ml]. This was tested by playing the game and 
    entering all the possible statuses a state could have, such as Walking,
    PokeCenter, Menu, Win, etc., and seeing if the GUI was correclty updated
    to reflect the change in status.

    This demonstrates correctness because the functions of [gui.ml] only
    contain rendering functions that must be checked on the GUI based on the
    different statuses checked in [main.ml]. We also tested invalid inputs,
    to ensure that these invalid inputs are caught and that a message is
    displayed to provide feedback to the player.

   - [state.ml] was tested manually by checking if the GUI rendered correctly
     for all possible statuses, as [state.ml] handles updating the status for
     all of the main functionality of the system, including spawning pokemon,
     walking, entering and walking in a gym, the pokecenter, and
     battling a trainer. 

    This demonstrates correctness as we are able to see that the state
    is correctly updating by how the GUI renders a new screen and thus the
    new state.

   - [initial.ml] was tested manually by initalizing a game and seeing if the
     correct messages were shown in the terminal and checking that the player's
     inputs were correctly used to initalize a game state. Again, this was
     tested by shwoing messages on the GUI, as we could check for the correct 
     starter pokemon, map, and player name.






*)

open OUnit2

let pokemon_tests = List.flatten [
    TestPokemon.type_from_string_tests;
    TestPokemon.opponent_move_tests;
    TestPokemon.battle_damage_tests;
    TestPokemon.level_up_tests;
    TestPokemon.increase_exp_tests;
  ]

let command_tests = List.flatten [
    TestCommand.parse_region_tests;
    TestCommand.parse_starter_tests;
    TestCommand.parse_yn_tests;
  ]

let block_tests = List.flatten [ TestBlock.block_tests; ]

let trainer_tests = List.flatten [ TestTrainer.trainer_tests; ]

let tests = List.flatten [
    pokemon_tests;
    command_tests;
    block_tests;
    trainer_tests;
  ]

let suite = "test suite for Pokaml"  >::: tests

let _ = run_test_tt_main suite