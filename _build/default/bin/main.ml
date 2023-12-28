open Final
open Character
open Story
open Pokemon
open Battle
open Yojson.Basic.Util

(*********** Constructing Story Plot ***********)
let load_storyplot address plot =
  let json = Yojson.Basic.from_file address in
  json |> member plot |> to_list |> List.map to_string

let intro =
  StoryPlot.construct_plot (load_storyplot "data/data.json" "initial_pokemon")

let gift_fromZ =
  StoryPlot.construct_plot (load_storyplot "data/data.json" "gift from Dr.Z")

let path1 = StoryPlot.construct_plot (load_storyplot "data/data.json" "path1")

let pallet_town =
  StoryPlot.construct_plot (load_storyplot "data/data.json" "Pallet Town")
(*when before construct_plot alter index of list for walking along town/forest
  and yes comp*)

let viridian_forest =
  StoryPlot.construct_plot (load_storyplot "data/data.json" "Viridian Forest")

let fork =
  StoryPlot.construct_plot (load_storyplot "data/data.json" "fork again")

let rand_info = load_storyplot "data/random.json" "RANDOM GYM INFO"

let iterate_plot plot decision player =
  StoryPlot.out_put !plot;
  print_string "> ";
  let choice = read_line () in
  decision := choice;
  plot := StoryPlot.make_decision !plot !decision !player

let process_key line =
  try
    let key_index = String.index line '@' in
    String.sub line (key_index + 1) (String.length line - key_index - 1)
  with Not_found -> line

(**************** Pokemon Game ****************)
let player = ref (Character.init "")
let plot = ref intro
let decision = ref "" (* User input*)
let pos = ref "" (*Used for recursion for determining location*)

let () =
  (* Welcome and initialize character*)
  print_endline "\n\nWelcome to our Pokemon Game\n";
  print_endline "Please enter your name:";
  print_string "> ";
  let name = read_line () in
  print_endline
    ("Hello " ^ name
   ^ "!!! Here are some keys to keep in mind before we get started: ");
  player := Character.init name;
  print_endline "O: Open Bag";
  print_endline "L: Open Your Current Pokemon List";
  print_endline "P: Open all Pokemon Dictionary";
  print_endline "S: Open Pokemon Dictionary to search a Pokemon";

  (* ---[ INTRO ]---*)
  for i = 0 to 1 do
    iterate_plot plot decision player
  done;
  (* Add selected pokemon to character's pokemon list*)
  let poke_name = StoryPlot.get_string !plot in
  let starter = StoryPlot.search_pokemon poke_name StoryPlot.starter_poke in
  player := Character.add_pokemon !player starter;
  print_endline
    ("\n Congrats on getting your first pokemon. Let's check out "
   ^ starter.name ^ " :");
  StoryPlot.view_pokemon starter;
  StoryPlot.pp_lines
    "\n\
     You're all set! Head onto your journey. Remember your mission is to \
     collect and train your pokemons in preparation for the final competiion!\n";

  (* ---[ GIFT ]---*)
  plot := gift_fromZ;
  iterate_plot plot decision player;
  let gift = StoryPlot.get_string !plot in
  let item =
    match gift with
    | "Purple bag" -> "potion"
    | "Yellow bag" -> "revive"
    | _ -> ""
  in
  print_endline ("You opened the gift and find a " ^ item);
  player := Character.add_item !player item 1;

  (* ---[ PATH 1 ]---*)
  plot := path1;
  for i = 0 to 1 do
    iterate_plot plot decision player
  done;
  (*Selection of place activates different actions and plots*)
  (let place = StoryPlot.get_string !plot in
   match place with
   | "Path with lots of tree" ->
       (*pick up a potion*)
       let item = "potion" in
       print_endline "\nYou discover a hidden potion nestled among the bushes.";
       player := Character.add_item !player item 1;
       plot := viridian_forest
   | "Path with lots of sunshine" ->
       (*earn a new pokemon*)
       let grookey = StoryPlot.search_pokemon "Grookey" Pokemon.masterlist in
       StoryPlot.pp_lines
         "\n\
          While traversing a lush route, you cross path with a curious \
          Grookey, who decided to join you on the adventure.";
       player := Character.add_pokemon !player grookey;
       print_endline "Welcome Grookey onto the team!";
       StoryPlot.view_pokemon grookey;
       plot := viridian_forest
   | "Yes" ->
       StoryPlot.pp_lines
         "After hearing your plan, your parents are excited about the journey. \
          They gave you a gift.";
       player := Character.add_item !player "revive" 1;
       print_endline "";
       plot := pallet_town
   | _ ->
       StoryPlot.pp_lines
         "You're eager to hit on your journey so you decided to visit next \
          time.";
       plot := pallet_town);

  (* ---[ FOREST/TOWN ]---*)
  while !decision <> "E" do
    pos := process_key (StoryPlot.get_string !plot);
    (match !pos with
    (*extract !pos for Random Walk forest*)
    | "RANDOM WALK CITY" -> (
        for i = 0 to 2 do
          iterate_plot plot decision player
        done;
        let place = StoryPlot.get_string !plot in
        match place with
        | "Of course!" ->
            let info =
              List.nth rand_info (Random.int (List.length rand_info))
            in
            StoryPlot.pp_lines
              ("Steve seems really interested about the gym and remarked: "
             ^ info)
        | "No, tell me about it" ->
            StoryPlot.pp_lines
              "A Pokemon Gym is a facility where trainers challenge leaders to \
               earn Badges and progress in their journey to become a Pokémon \
               Master."
        | "Fight!" -> (
            print_endline "You started a battle with Roger";
            player :=
              match Battle.battle player with
              | Win char -> char
              | Lose _ -> !player)
        | "I'm not ready for that..." ->
            print_endline "Roger mocked you and left..."
        | "Yes, please" -> player := Character.heal_all !player
        | "No, I'm good" -> print_endline "You left the hospital"
        | "Yes" -> player := Character.add_item !player "potion" 1
        | "No" -> print_endline "You left the shop"
        | _ -> ())
    | "RANDOM WALK FOREST" -> (
        (*reinitiate plot for random*)
        for i = 0 to 1 do
          iterate_plot plot decision player
        done;
        let place = StoryPlot.get_string !plot in
        match place with
        | "Throw rock" -> (
            print_endline "You startled the pokemon and it started a battle.";
            player :=
              match Battle.battle player with
              | Win char -> char
              | Lose _ -> !player)
        | "Observe" ->
            print_endline
              "As you're deciding what to do, the pokemon escaped. But you \
               find a potion in the bush.";
            player := Character.add_item !player "potion" 1
        | "No" -> (
            print_endline
              "While you're walking, a pokemon attacked you and started a \
               battle";
            player :=
              match Battle.battle player with
              | Win char -> char
              | Lose _ -> !player)
        | "Yes" -> print_endline "You enjoyed the nice weather"
        | _ -> ())
    | _ -> () (*deal with later for infinite loop*));
    print_endline "\n \n [If you want to end game now, type E]";
    if !decision <> "E" then (
      plot := fork;
      iterate_plot plot decision player;
      match StoryPlot.get_string !plot with
      | "Viridian Forest" -> plot := viridian_forest
      | _ -> plot := pallet_town)
    else print_endline "Game End"
  done
