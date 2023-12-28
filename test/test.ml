open Final
open OUnit2
include Pokemon
include Skill
include Character
include Battle

(* 
   Test plan for the Final Project:
   The majority of the test cases for the Final Project are OUnit tests. We have 
   created a test helper function called test_helper to help with the abstraction
   of the tests. For some of the tests inside the battle module, we have implemented
   some manual tests. Those tests require player input and are not automated. 
   For the ease of testing, we have commented out those tests in this file. 
   For other tests, including the tests for the functions in battle, character, 
   pokemon, and story module, the tests are automated through OUnit. We also 
   did some end-to-end testing for the entire game by achieving converage for 
   each branch in the game. The tests were implemented via test driven development.
   For the majority of the tests, we implemented the tests via black box, but we 
   also added some glass box testing to ensure program correctness. 
  
   Our test approach demonstrates the correctness of the system. The integration of both 
   automated, manual, and end-to-end testing si the key aspect of our testing strategy.
   The automated tests rigorously evaluate the majority of our modules, providing
   quick and reliable validations. THe manual test complements this by assessing 
   user interaction scenarios. The end-to-end tests ensure that the entire game 
   is working as expected. 

*)
let test_helper name expected_output input =
  name >:: fun _ -> assert_equal expected_output input

let pokemon1 = Story.StoryPlot.search_pokemon "Pikachu" Pokemon.masterlist
let pokemon2 = Story.StoryPlot.search_pokemon "Bulbasaur" Pokemon.masterlist
let pokemon3 = Story.StoryPlot.search_pokemon "Charmander" Pokemon.masterlist
let pokemon4 = Story.StoryPlot.search_pokemon "Squirtle" Pokemon.masterlist
let character1 = Character.init "Ash"
let thunder_shock = Skill.create_skill "Thunder Shock" 30 Electric
let poison_fang = Skill.create_skill "Poison Fang" 30 Poison
let poison_point = Skill.create_skill "Poison Point" 25 Poison
let vine_whip = Skill.create_skill "Vine Whip" 25 Grass
let razor_leaf = Skill.create_skill "Razor Leaf" 30 Grass
let poison_sting = Skill.create_skill "Poison Sting" 30 Poison
let water_pulse = Skill.create_skill "Water Pulse" 25 Water
let nuzzle = Skill.create_skill "Nuzzle" 20 Electric
let tail_whip = Skill.create_skill "Tail Whip" 20 Normal
let play_nice = Skill.create_skill "Play Nice" 10 Normal
let pikachu_skills = [ thunder_shock ]
let nidoran_skills = [ poison_fang; poison_point ]
let bulbasaur_skills = [ vine_whip ]
let turtwig_skills = [ razor_leaf ]
let ekans_skills = [ poison_sting ]
let psyduck_skills = [ water_pulse ]
let pichu_skills = [ thunder_shock; nuzzle; tail_whip; play_nice ]

let pikachu =
  Pokemon.construct_pokemon "Pikachu" 40 40 1 0 Electric pikachu_skills

let nidoran =
  Pokemon.construct_pokemon "Nidoran" 55 55 5 0 Poison nidoran_skills

let bulbasaur =
  Pokemon.construct_pokemon "Bulbasaur" 45 45 3 0 Grass bulbasaur_skills

let turtwing = Pokemon.construct_pokemon "Turtwig" 0 35 5 0 Grass turtwig_skills
let ekans = Pokemon.construct_pokemon "Ekans" 10 40 2 0 Poison ekans_skills
let psyduck = Pokemon.construct_pokemon "Psyduck" 0 45 4 0 Water psyduck_skills
let pichu = Pokemon.construct_pokemon "Pichu" 20 45 3 0 Electric pichu_skills
let pokemonList = [ pikachu; nidoran; bulbasaur; turtwing; ekans; psyduck ]

let battle_tests =
  [
    ( "pokemon_masterlist all are positive" >:: fun _ ->
      let masterlist = Battle.posHP_pokemon Pokemon.masterlist in
      assert_equal 32 (List.length masterlist) );
    ( "pokemon_list initial length" >:: fun _ ->
      assert_equal 6 (List.length pokemonList) );
    ( "pokemon_list check new length" >:: fun _ ->
      let result = Battle.posHP_pokemon pokemonList in
      assert_equal 4 (List.length result) );
    ( "pokemon_list check ekans" >:: fun _ ->
      let checking = ekans in
      let result = Battle.posHP_pokemon pokemonList in
      assert_equal true (List.mem checking result) );
    ( "pokemon_list check psyduck" >:: fun _ ->
      let checking = psyduck in
      let result = Battle.posHP_pokemon pokemonList in
      assert_equal false (List.mem checking result) );
    (* ( "choosing pokemonlist checkname3" >:: fun _ -> let pokemonList = [
       pikachu; nidoran; bulbasaur; turtwing; ekans; psyduck ] in let
       selected_pokemon = print_endline "[Choose 3]"; Battle.choose_pokemon
       pokemonList in assert_equal "Bulbasaur" selected_pokemon.name ); (*input
       3 *) ( "choosing pokemonlist checkhpmax4" >:: fun _ -> let pokemonList =
       [ pikachu; nidoran; bulbasaur; turtwing; ekans; psyduck ] in let
       selected_pokemon = print_endline "[Choose 4]"; Battle.choose_pokemon
       pokemonList in assert_equal 40 selected_pokemon.hp_max ); (*input 4*) (
       "choosing masterlist name1" >:: fun _ -> let selected_pokemon =
       print_endline "[Choose 1]"; Battle.choose_pokemon Pokemon.masterlist in
       assert_equal "Minun" selected_pokemon.name ); (*input 1*) *)
    ( "generating a pokemon" >:: fun _ ->
      let generated_pokemon = Battle.generate_pokemon Pokemon.masterlist in
      assert_equal true
        (List.exists (fun p -> p = generated_pokemon) Pokemon.masterlist) );
    ( "generating checking psyduck" >:: fun _ ->
      let is_psyduck_in_mylist =
        List.exists (fun p -> p = psyduck) pokemonList
      in
      assert_equal true is_psyduck_in_mylist );
    ( "generating checking psyduck in poshp" >:: fun _ ->
      let result = Battle.posHP_pokemon pokemonList in
      let is_psyduck_in_mylist = List.exists (fun p -> p = psyduck) result in
      assert_equal false is_psyduck_in_mylist );
    (* ( " battle" >:: fun _ -> let character1 = ref (Character.init "Ash") in
       character1 := Character.add_pokemon !character1 pokemon1; ignore (battle
       character1); assert (pokemon1.hp_curr < pokemon1.hp_max) ); *)
  ]

let pokemon_tests =
  [
    ( "skills_master_lst include all" >:: fun _ ->
      assert_equal 54 (List.length Skill.skills_master_lst) );
    ( "create_skill with accurate info" >:: fun _ ->
      let howl = Skill.create_skill "howl" 10 Normal in
      assert_equal Skill.{ name = "howl"; damage = 10; element = Normal } howl
    );
    ( "get_skill for valid skill" >:: fun _ ->
      assert_equal
        Skill.{ name = "Electro Ball"; damage = 20; element = Electric }
        (Skill.get_skill "Electro Ball") );
    ( "get_skill for invalid skill" >:: fun _ ->
      assert_raises Not_found (fun () -> Skill.get_skill "Spy") );
    ( "masterlist include all" >:: fun _ ->
      assert_equal 32 (List.length Pokemon.masterlist) );
    ( "construct_pokemon with accurate info" >:: fun _ ->
      let spy = Skill.create_skill "spy" 20 Ghost in
      let loopy = Pokemon.construct_pokemon "Loopy" 40 50 5 0 Normal [ spy ] in
      assert_equal
        Pokemon.
          {
            name = "Loopy";
            hp_curr = 40;
            hp_max = 50;
            level = 5;
            xp = 0;
            elem_type = Normal;
            skills = [ spy ];
          }
        loopy );
    ( "construct_pokemon hp_curr = hp_max" >:: fun _ ->
      let spy = Skill.create_skill "spy" 20 Ghost in
      let loopy = Pokemon.construct_pokemon "Loopy" 50 50 5 0 Normal [ spy ] in
      assert_equal
        Pokemon.
          {
            name = "Loopy";
            hp_curr = 50;
            hp_max = 50;
            level = 5;
            xp = 0;
            elem_type = Normal;
            skills = [ spy ];
          }
        loopy );
    ( "construct_pokemon failure for negative hp_curr" >:: fun _ ->
      assert_raises (Failure "HP cannot be negative") (fun () ->
          Pokemon.construct_pokemon "Bulbsaur" ~-20 20 1 0 Grass [ vine_whip ])
    );
    ( "construct_pokemon failure for negative hp_max" >:: fun _ ->
      assert_raises (Failure "HP cannot be negative") (fun () ->
          Pokemon.construct_pokemon "Bulbsaur" 20 ~-20 1 0 Grass [ vine_whip ])
    );
    ( "construct_pokemon failure for hp_curr > max_hp" >:: fun _ ->
      assert_raises (Failure "Current HP cannot be greater than max HP")
        (fun () ->
          Pokemon.construct_pokemon "Bulbsaur" 30 20 1 0 Grass [ vine_whip ]) );
    ( "construct_pokemon failure invalid level" >:: fun _ ->
      assert_raises (Failure "Level must be at least 1") (fun () ->
          Pokemon.construct_pokemon "Bulbsaur" 20 20 0 0 Grass [ vine_whip ]) );
    ( "construct_pokemon failure invalid xp" >:: fun _ ->
      assert_raises (Failure "XP must be at least 0") (fun () ->
          Pokemon.construct_pokemon "Bulbsaur" 20 20 1 ~-20 Grass [ vine_whip ])
    );
    ( "construct_pokemon failure no skill" >:: fun _ ->
      assert_raises (Failure "Pokemon must have at lease one skill") (fun () ->
          Pokemon.construct_pokemon "Bulbsaur" 20 20 1 0 Grass []) );
    ( "attack reduces hp" >:: fun _ ->
      let spy = Skill.create_skill "spy" 20 Ghost in
      let loopy = Pokemon.construct_pokemon "Loopy" 40 50 5 0 Normal [ spy ] in
      let pororo =
        Pokemon.construct_pokemon "Pororo" 40 50 5 0 Normal [ spy ]
      in
      Pokemon.attack loopy pororo spy;
      assert (pororo.Pokemon.hp_curr <= 40);
      assert (loopy.Pokemon.hp_curr = 40) );
    ( "attack reduces hp by random int if skill no base damage" >:: fun _ ->
      let spy = Skill.create_skill "spy" 0 Ghost in
      let loopy = Pokemon.construct_pokemon "Loopy" 40 50 5 0 Normal [ spy ] in
      let pororo =
        Pokemon.construct_pokemon "Pororo" 40 50 5 0 Normal [ spy ]
      in
      Pokemon.attack loopy pororo spy;
      assert (pororo.Pokemon.hp_curr <= 40 && pororo.Pokemon.hp_curr >= 36);
      assert (loopy.Pokemon.hp_curr = 40) );
    ( "defend cause damage to own" >:: fun _ ->
      let defend = Skill.create_skill "spy" 20 Ghost in
      let loopy =
        Pokemon.construct_pokemon "Loopy" 40 50 5 0 Normal [ defend ]
      in
      let pororo =
        Pokemon.construct_pokemon "Pororo" 40 50 5 0 Normal [ defend ]
      in
      Pokemon.defend loopy pororo defend;
      assert (loopy.Pokemon.hp_curr = 20);
      assert (pororo.Pokemon.hp_curr = 40) );
    ( "level_up check hp increase" >:: fun _ ->
      let spy = Skill.create_skill "spy" 20 Ghost in
      let loopy = Pokemon.construct_pokemon "Loopy" 50 50 5 0 Normal [ spy ] in
      let new_loopy = Pokemon.level_up loopy in
      assert (new_loopy.xp >= loopy.xp) );
    ( "level_up check hp increase" >:: fun _ ->
      let spy = Skill.create_skill "spy" 20 Ghost in
      let loopy =
        ref (Pokemon.construct_pokemon "Loopy" 50 50 5 0 Normal [ spy ])
      in
      for i = 0 to 10 do
        loopy := Pokemon.level_up !loopy
      done );
    ( "level_up assert increment of 1" >:: fun _ ->
      let rand = 3 in
      let pikachu =
        Pokemon.construct_pokemon "Pikachu" 40 40 rand 20 Electric
          pikachu_skills
      in
      let new_pikachu = Pokemon.level_up pikachu in
      assert (new_pikachu.level - pikachu.level = 1) );
    ( "type_advantage return empty" >:: fun _ ->
      assert_equal [] (Pokemon.type_advantage Normal) );
    ( "type_advantage return nonempty" >:: fun _ ->
      assert_equal [ Grass ] (Pokemon.type_advantage Fire) );
    ( "type_disadvantage return empty" >:: fun _ ->
      assert_equal [] (Pokemon.type_disadvantage Normal) );
    ( "type_disadvantage return nonempty" >:: fun _ ->
      assert_equal [ Water ] (Pokemon.type_disadvantage Fire) );
    ( "attack increase damage for pokemon with advantage" >:: fun _ ->
      let fire = Story.StoryPlot.search_pokemon "Ponyta" Pokemon.masterlist in
      let grass =
        Story.StoryPlot.search_pokemon "Bellossom" Pokemon.masterlist
      in
      let grass_hp = grass.hp_curr in
      let flame_wheel = Skill.get_skill "Flame Wheel" in
      Pokemon.attack fire grass flame_wheel;
      assert_equal true (flame_wheel.damage <= grass_hp - grass.hp_curr) );
    ( "attack reduced damage for pokemon with disadvantage" >:: fun _ ->
      let fire = Story.StoryPlot.search_pokemon "Ponyta" Pokemon.masterlist in
      let grass =
        Story.StoryPlot.search_pokemon "Bellossom" Pokemon.masterlist
      in
      let fire_hp = fire.hp_curr in
      let petal_dance = Skill.get_skill "Petal Dance" in
      Pokemon.attack grass fire petal_dance;
      assert_equal true (petal_dance.damage >= fire_hp - fire.hp_curr) );
    ( "attack in two rounds" >:: fun _ ->
      let minun = Story.StoryPlot.search_pokemon "Minun" Pokemon.masterlist in
      let pichu = Story.StoryPlot.search_pokemon "Pichu" Pokemon.masterlist in
      Pokemon.attack pichu minun (List.hd pichu.skills);
      assert (minun.Pokemon.hp_curr <= minun.Pokemon.hp_max);
      assert (pichu.Pokemon.hp_curr = pichu.Pokemon.hp_max);
      Pokemon.attack minun pichu (List.hd minun.skills);
      assert (pichu.Pokemon.hp_curr <= pichu.Pokemon.hp_max);
      assert (minun.Pokemon.hp_curr <= minun.Pokemon.hp_max) );
    ( "extended test_skills_creation" >:: fun _ ->
      let thunder_shock = Skill.create_skill "Thunder Shock" 20 Electric in
      assert (
        thunder_shock.Skill.name = "Thunder Shock"
        && thunder_shock.Skill.damage = 20
        && thunder_shock.Skill.element = Electric);

      let poison_fang = Skill.create_skill "Poison Fang" 30 Poison in
      assert (
        poison_fang.Skill.name = "Poison Fang"
        && poison_fang.Skill.damage = 30
        && poison_fang.Skill.element = Poison);

      let poison_point = Skill.create_skill "Poison Point" 25 Poison in
      assert (
        poison_point.Skill.name = "Poison Point"
        && poison_point.Skill.damage = 25
        && poison_point.Skill.element = Poison);

      let vine_whip = Skill.create_skill "Vine Whip" 25 Grass in
      assert (
        vine_whip.Skill.name = "Vine Whip"
        && vine_whip.Skill.damage = 25
        && vine_whip.Skill.element = Grass);

      let pikachu_skills = [ thunder_shock ] in
      let nidoran_skills = [ poison_fang; poison_point ] in
      let bulbasaur_skills = [ vine_whip ] in

      let pikachu =
        Pokemon.construct_pokemon "Pikachu" 20 20 1 0 Electric pikachu_skills
      in
      let nidoran =
        Pokemon.construct_pokemon "Nidoran" 25 25 5 0 Poison nidoran_skills
      in
      let bulbasaur =
        Pokemon.construct_pokemon "Bulbasaur" 25 25 3 0 Grass bulbasaur_skills
      in

      let x = nidoran.Pokemon.hp_curr in
      Pokemon.attack pikachu nidoran thunder_shock;
      let y = nidoran.Pokemon.hp_curr in
      assert_equal true (x >= y);

      let x2 = pikachu.Pokemon.hp_curr in
      Pokemon.attack nidoran pikachu poison_fang;
      let y2 = pikachu.Pokemon.hp_curr in
      assert (x2 >= y2);

      let x3 = pikachu.Pokemon.hp_curr in
      Pokemon.attack nidoran pikachu poison_point;
      let y3 = pikachu.Pokemon.hp_curr in
      assert (x3 >= y3);

      (*damage is randomized, apply attack multiple times to ensure y4=0*)
      for i = 1 to 10 do
        Pokemon.attack nidoran pikachu poison_fang
      done;
      let y4 = pikachu.Pokemon.hp_curr in
      Pokemon.attack nidoran pikachu poison_fang;
      assert (y4 = 0);

      let x5 = nidoran.Pokemon.hp_curr in
      Pokemon.attack bulbasaur nidoran vine_whip;
      let y5 = nidoran.Pokemon.hp_curr in
      assert (x5 >= y5) );

    ( "type_advantage1" >:: fun _ ->
      assert_equal [ Grass ] (Pokemon.type_advantage Fire) );
    ("type _advantage2" >:: fun _ ->
      assert_equal [ Fire ] (Pokemon.type_advantage Water) );
    ( "type_advantage3" >:: fun _ ->
      assert_equal [ Water ] (Pokemon.type_advantage Grass) );
    ( "type_advantage4" >:: fun _ ->
      assert_equal [ Grass ] (Pokemon.type_advantage Poison) );
    ( "type_advantage5" >:: fun _ ->
      assert_equal [ Water; Flying ] (Pokemon.type_advantage Electric) );
    (" type_advantage6" >:: fun _ ->
      assert_equal [Poison] (Pokemon.type_advantage Flying) );
    ("type_advantage7" >:: fun _ ->
      assert_equal [] (Pokemon.type_advantage Normal) );
    ("type_advantage8" >:: fun _ ->
      assert_equal [Ghost] (Pokemon.type_advantage Ghost) );

    ( "type_disadvantage1" >:: fun _ ->
      assert_equal [Water] (Pokemon.type_disadvantage Fire) );
    ("type_disadvantage2" >:: fun _ ->
      assert_equal [Grass] (Pokemon.type_disadvantage Water) );
    ("type_disadvantage3" >:: fun _ ->
      assert_equal [Fire; Poison] (Pokemon.type_disadvantage Grass) );
    ("type_disadvantage4" >:: fun _ ->
      assert_equal [Flying] (Pokemon.type_disadvantage Poison) );
    ("type_disadvantage5" >:: fun _ ->
      assert_equal [] (Pokemon.type_disadvantage Electric) );
    ("type_disadvantage6" >:: fun _ ->
      assert_equal [Electric] (Pokemon.type_disadvantage Flying) );
    ("type_disadvantage7" >:: fun _ ->
      assert_equal [] (Pokemon.type_disadvantage Normal) );
    ("type_disadvantage8" >:: fun _ ->
      assert_equal [Ghost] (Pokemon.type_disadvantage Ghost) );
    (" elem_of_string1" >:: fun _ ->
      assert_equal Fire (elem_of_string "Fire") );
    ("elem_of_string2" >:: fun _ ->
      assert_equal Water (elem_of_string "Water") );
    ("elem_of_string3" >:: fun _ ->
      assert_equal Grass (elem_of_string "Grass") );
    ("elem_of_string4" >:: fun _ ->
      assert_equal Poison (elem_of_string "Poison") );
    ("elem_of_string5" >:: fun _ ->
      assert_equal Electric (elem_of_string "Electric") );
    ("elem_of_string6" >:: fun _ ->
      assert_equal Flying (elem_of_string "Flying") );
    ("elem_of_string7" >:: fun _ ->
      assert_equal Normal (elem_of_string "Normal") );
    ("string_of_elem1" >:: fun _ ->
      assert_equal "Fire" (string_of_elem Fire) );
    ("string_of_elem2" >:: fun _ ->
      assert_equal "Water" (string_of_elem Water) );
    ("string_of_elem3" >:: fun _ ->
      assert_equal "Grass" (string_of_elem Grass) );
    ("string_of_elem4" >:: fun _ ->
      assert_equal "Poison" (string_of_elem Poison) );
    ("string_of_elem5" >:: fun _ ->
      assert_equal "Electric" (string_of_elem Electric) );
    ("string_of_elem6" >:: fun _ ->
      assert_equal "Flying" (string_of_elem Flying) );
    ("string_of_elem7" >:: fun _ ->
      assert_equal "Normal" (string_of_elem Normal) );
    ("string_of_elem8" >:: fun _ ->
      assert_equal "Ghost" (string_of_elem Ghost) );

    


  ]

let character_tests =
  [
    test_helper "1. init: check name" "Ash" character1.character_name;
    test_helper "2. init: check pokemon list" [] character1.pokemon_list;
    test_helper "3. add_pokemon" 1
      ((Character.add_pokemon character1 pokemon1).pokemon_list |> List.length);
    test_helper "add pokemon 2" [ pokemon1; pokemon2 ]
      (Character.add_pokemon
         (Character.add_pokemon character1 pokemon2)
         pokemon1)
        .pokemon_list;
    test_helper "add duplicate pokemon" [ pokemon1 ]
      (Character.add_pokemon
         (Character.add_pokemon character1 pokemon1)
         pokemon1)
        .pokemon_list;
    test_helper "add pokemon complex" [ pokemon2; pokemon1 ]
      (Character.add_pokemon
         (Character.add_pokemon
            (Character.add_pokemon character1 pokemon1)
            pokemon2)
         pokemon1)
        .pokemon_list;
    test_helper " add_remove pair" 0
      ((Character.remove_pokemon
          (Character.add_pokemon character1 pokemon1)
          "Pikachu")
         .pokemon_list |> List.length);
    test_helper " add_get pair" pokemon1.name
      (Character.get_pokemon
         (Character.add_pokemon character1 pokemon1)
         "Pikachu")
        .name;
    test_helper "get_pokemon" pokemon1.name
      (Character.get_pokemon
         (Character.add_pokemon character1 pokemon1)
         "Pikachu")
        .name;
    test_helper "get_pokemon2" pokemon2.name
      (Character.get_pokemon
         (Character.add_pokemon character1 pokemon2)
         "Bulbasaur")
        .name;
    test_helper "add_item" 1
      (Character.add_item character1 "potion" 1).backpack.potions;
    test_helper "add_item" 2
      (Character.add_item (Character.add_item character1 "potion" 1) "potion" 1)
        .backpack
        .potions;
    test_helper "add_item" 10
      (Character.add_item character1 "potion" 10).backpack.potions;
    ( "test_character_initialization" >:: fun _ ->
      let character = Character.init "Ash" in
      assert (character.Character.character_name = "Ash");
      assert (character.Character.pokemon_list = []);
      assert (character.Character.backpack.BackPack.current_capacity = 0);
      assert (character.Character.backpack.BackPack.max_capacity = 10) );
    ( "test_add_pokemon" >:: fun _ ->
      let character = Character.init "Ash" in
      let updated_character = Character.add_pokemon character pikachu in
      assert (List.hd updated_character.Character.pokemon_list = pikachu) );
    ( "test_remove_nonexisting_pokemon" >:: fun _ ->
      let character = Character.init "Ash" in
      let character1 = Character.remove_pokemon character "cake" in
      assert (character.pokemon_list = character1.pokemon_list) );
    ( "test_remove_pokemon" >:: fun _ ->
      let character = Character.init "Ash" in
      let character = Character.add_pokemon character pikachu in
      let updated_character = Character.remove_pokemon character "Pikachu" in
      assert (
        not
          (List.exists
             (fun p -> p.Pokemon.name = "Pikachu")
             updated_character.Character.pokemon_list)) );
    ( "test_get_pokemon" >:: fun _ ->
      let character = Character.init "Ash" in
      let character = Character.add_pokemon character pikachu in
      let retrieved_pokemon = Character.get_pokemon character "Pikachu" in
      assert (retrieved_pokemon = pikachu) );
    ( "test_add_remove_items" >:: fun _ ->
      let character = Character.init "Ash" in
      let character_with_potion = Character.add_item character "potion" 1 in
      assert (character_with_potion.Character.backpack.BackPack.potions = 1);
      let character_no_potion =
        Character.remove_item character_with_potion "potion"
      in
      assert (character_no_potion.Character.backpack.BackPack.potions = 0) );
    ( "test_backpack_boundaries when at max" >:: fun _ ->
      let character = Character.init "Ash" in
      let character_max_items = Character.add_item character "potion" 10 in
      assert_equal 10
        (Character.add_item character_max_items "potion" 1).backpack.potions );
    ( "test_backpack_boundaries when at min" >:: fun _ ->
      let character = Character.init "Ash" in
      assert_equal 10
        (Character.add_item character "potion" 10).backpack.potions );
    ( "test_backpack_boundaries when at middle" >:: fun _ ->
      let character = Character.init "Ash" in
      let character_max_items = Character.add_item character "potion" 5 in
      assert_equal 10
        (Character.add_item character_max_items "potion" 5).backpack.potions );
    ( "test_backpack_boundaries that cause overflow" >:: fun _ ->
      let character = Character.init "Ash" in
      let character_max_items = Character.add_item character "potion" 5 in
      assert_equal 10
        (Character.add_item character_max_items "potion" 10).backpack.potions );
    ( "test_backpack_boundaries negative amount" >:: fun _ ->
      let character = Character.init "Ash" in
      let character_max_items = Character.add_item character "potion" 10 in
      assert_raises (Failure "Error: Amount cannot be negative") (fun () ->
          Character.add_item character_max_items "potion" ~-1) );
    ( "test_backpack_boundaries negative amount" >:: fun _ ->
      let character = Character.init "Ash" in
      let character_max_items = Character.add_item character "potion" 10 in
      assert_raises (Failure "Error: Amount cannot be negative") (fun () ->
          Character.add_item character_max_items "potion" ~-5) );
    ( "test_duplicate_pokemon_handling" >:: fun _ ->
      let character = Character.init "Ash" in
      let character = Character.add_pokemon character pikachu in
      let character = Character.add_pokemon character pikachu in
      assert (List.length character.Character.pokemon_list = 1);
      let character_one_removed =
        Character.remove_pokemon character "Pikachu"
      in
      assert (List.length character_one_removed.Character.pokemon_list = 0) );
    ( "test_internal_state_changes" >:: fun _ ->
      let character = Character.init "Ash" in
      let character_with_potion = Character.add_item character "potion" 1 in
      assert (
        character_with_potion.Character.backpack.BackPack.current_capacity = 1);
      let character_no_potion =
        Character.remove_item character_with_potion "potion"
      in
      assert (
        character_no_potion.Character.backpack.BackPack.current_capacity = 0) );
    ( "test_exception_handling" >:: fun _ ->
      let character = Character.init "Ash" in
      assert_raises Not_found (fun () ->
          Character.get_pokemon character "Missingno");
      assert_raises (Failure "Invalid item") (fun () ->
          Character.add_item character "UnknownItem" 1);
      assert_raises (Failure "Error: Current capacity can't be negative!")
        (fun () -> Character.remove_item character "potion") );
    ( "test_interaction_with_pokemon_module" >:: fun _ ->
      let character = Character.init "Ash" in
      let character = Character.add_pokemon character pikachu in
      let pikachu_retrieved = Character.get_pokemon character "Pikachu" in
      assert (pikachu_retrieved = pikachu) );
    ( "heal_all check pokemon hp healed" >:: fun _ ->
      let pika =
        Pokemon.construct_pokemon "Pikachu" 20 40 1 0 Electric pikachu_skills
      in
      let old = Character.add_pokemon (Character.init "Ash") pika in
      let new_char = Character.heal_all (Character.add_pokemon old pika) in
      assert_equal 20 (List.hd old.pokemon_list).hp_curr;
      assert_equal 40 (List.hd new_char.pokemon_list).hp_curr );
    
    ( "heal_all check all pokemon in list healed" >:: fun _ ->
      let pachirisu =
        Pokemon.construct_pokemon "Pachirisu" 60 60 2 0 Electric
          [ thunder_shock ]
      in

      let minun =
        Pokemon.construct_pokemon "Minun" 0 60 1 0 Poison [ poison_fang ]
      in

      let plusle =
        Pokemon.construct_pokemon "Plusle" 40 60 3 0 Electric [ thunder_shock ]
      in
      let before =
        Character.
          { (init "Ash") with pokemon_list = [ pachirisu; minun; plusle ] }
      in
      let after = Character.heal_all before in
      assert (
        List.for_all
          (fun pok -> pok.Pokemon.hp_curr = pok.Pokemon.hp_max)
          after.pokemon_list);
      assert (
        List.exists
          (fun pok -> pok.Pokemon.hp_curr != pok.Pokemon.hp_max)
          before.pokemon_list) );
    ( "heal_all returns player if pokemon list is empty" >:: fun _ ->
      let before = Character.init "Ash" in
      let after = Character.heal_all before in
      assert_equal before.pokemon_list after.pokemon_list;
      assert_equal before.backpack after.backpack;
      assert_equal before.character_name after.character_name );
  ]

let story_tests =
  [
    test_helper "construct_plot: empty string list" (Story.Leaf "End")
      (Story.StoryPlot.construct_plot []);
    test_helper "construct_plot: non empty string list"
      (Story.Node ("Hello World", Story.Leaf "End", Story.Leaf "End"))
      (Story.StoryPlot.construct_plot [ "Hello World" ]);
    test_helper "get_string: Leaf" "End"
      (Story.StoryPlot.get_string (Story.StoryPlot.construct_plot []));
    test_helper "get_string: Node" "Hello World"
      (Story.StoryPlot.get_string
         (Story.StoryPlot.construct_plot [ "Hello World" ]));
    test_helper "make_decision: choosing A"
      (Story.StoryPlot.make_decision
         (Story.StoryPlot.construct_plot
            [
              "Hello! Young warrior! May I ask, did you drop this golden ax or \
               this silver ax?";
              "A: Golden ax";
              "B: Silver ax";
            ])
         "A" (Character.init "Ash"))
      (Story.StoryPlot.construct_plot [ "A: Golden ax" ]);
    test_helper "make_decision: choosing B"
      (Story.StoryPlot.make_decision
         (Story.StoryPlot.construct_plot
            [
              "Hello! Young warrior! May I ask, did you drop this golden ax or \
               this silver ax?";
              "A: Golden ax";
              "B: Silver ax";
            ])
         "B" (Character.init "Ash"))
      (Story.StoryPlot.construct_plot [ "B: Silver ax" ]);
    test_helper "make_decision: choosing to Exit"
      (Story.StoryPlot.make_decision
         (Story.StoryPlot.construct_plot
            [
              "Hello! Young warrior! May I ask, did you drop this golden ax or \
               this silver ax?";
              "A: Golden ax";
              "B: Silver ax";
            ])
         "E" (Character.init "Ash"))
      (Story.Leaf "End");
    test_helper "search_Pokemon: does exist in the list"
      (List.hd Story.StoryPlot.starter_poke)
      (Story.StoryPlot.search_pokemon "Pikachu" Story.StoryPlot.starter_poke);
    test_helper "output: Leaf" () (Story.StoryPlot.out_put (Story.Leaf "End"));
    test_helper "output: Node" ()
      (Story.StoryPlot.out_put
         (Story.StoryPlot.construct_plot
            [
              "Hello! Young warrior! May I ask, did you drop this golden ax or \
               this silver ax?";
              "A: Golden ax";
              "B: Silver ax";
            ]));
    test_helper "view_Pokemon" ()
      (Story.StoryPlot.view_pokemon (List.hd Story.StoryPlot.starter_poke));
    test_helper "view_Pokemon_lst" ()
      (Story.StoryPlot.pp_pokemon_lst Story.StoryPlot.starter_poke);
    ( "construct_plot leaf" >:: fun _ ->
      assert_equal (Story.Leaf "End") (Story.StoryPlot.construct_plot []) );
    ( "construct_plot single node" >:: fun _ ->
      assert_equal
        (Story.Node ("Intro", Story.Leaf "End", Story.Leaf "End"))
        (Story.StoryPlot.construct_plot [ "Intro" ]) );
    ( "construct_plot nodes" >:: fun _ ->
      assert_equal
        (Story.Node
           ( "Intro",
             Story.Node ("A", Story.Leaf "End", Story.Leaf "End"),
             Story.Node ("B", Story.Leaf "End", Story.Leaf "End") ))
        (Story.StoryPlot.construct_plot [ "Intro"; "A"; "B" ]) );
    ( "make_decision leaf, leaf regarless of decision" >:: fun _ ->
      let rec try_inputs list =
        match list with
        | [] -> ()
        | h :: t ->
            assert_equal (Story.Leaf "End")
              (Story.StoryPlot.make_decision
                 (Story.StoryPlot.construct_plot [])
                 h (Character.init "Ash"));
            try_inputs t
      in
      try_inputs [ "A"; "B"; "O"; "L"; "P"; "S"; "E" ] );
    ( "get_string leaf" >:: fun _ ->
      assert_equal "End"
        (Story.StoryPlot.get_string (Story.StoryPlot.construct_plot [])) );
    ( "get_string single node" >:: fun _ ->
      assert_equal "Intro"
        (Story.StoryPlot.get_string
           (Story.StoryPlot.construct_plot [ "Intro" ])) );
    ( "get_string nodes" >:: fun _ ->
      assert_equal "Intro"
        (Story.StoryPlot.get_string
           (Story.StoryPlot.construct_plot [ "Intro"; "A"; "B" ])) );
    ( "starter_poke length" >:: fun _ ->
      assert_equal 4 (List.length Story.StoryPlot.starter_poke) );
    ( "search_pokemon for valid pokemon" >:: fun _ ->
      assert_equal
        Pokemon.
          {
            name = "Minun";
            hp_curr = 60;
            hp_max = 60;
            level = 1;
            xp = 0;
            elem_type = Electric;
            skills =
              [
                Skill.get_skill "Nuzzle";
                Skill.get_skill "Thunder Wave";
                Skill.get_skill "Growl";
                Skill.get_skill "Spark";
              ];
          }
        (Story.StoryPlot.search_pokemon "Minun" Pokemon.masterlist) );
    ( "search_pokemon for invalid pokemon" >:: fun _ ->
      assert_raises Not_found (fun () ->
          Story.StoryPlot.search_pokemon "minun" Pokemon.masterlist) );
  ]

let suite =
  "Pokemon Game test suite"
  >::: List.flatten
         [ pokemon_tests; character_tests; story_tests; battle_tests ]

let _ = run_test_tt_main suite
