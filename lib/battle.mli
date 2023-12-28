open Pokemon
open Character

type battle_result =
  | Win of Character.character
  | Lose of unit

val posHP_pokemon : Pokemon.pokemon list -> Pokemon.pokemon list
(** [posHP_pokemon lst] return a list of pokemons from [lst] that have positive
    HP *)

val choose_pokemon : Pokemon.pokemon list -> Pokemon.pokemon
(** [choose_pokemon lst] ask for user input and return the pokemon that the user
    chooses from [lst]. If invalid input, then ask user to input again. If no
    pokemon in [lst] with positive hp, then raise [Not_Found]*)

val generate_pokemon : Pokemon.pokemon list -> Pokemon.pokemon
(** [generate_pokemon lst] randomly pick a pokemon from [lst]*)

val take_turn :
  Character.character -> Pokemon.pokemon -> Pokemon.pokemon -> unit
(** [take_turn player pokemon1 pokemon2] allows user to input number for
    choosing skills performed by [pokemon1] (own pokemon). Skills for [pokemon2]
    is randomly selected. If invalid input, then ask user to input again.*)

val battle : Character.character ref -> battle_result
(** [battle character] execute the battle process, where both pokemon will
    attack to each other. If [character] has no pokemon with positive hp, the
    battle process will not proceed and print message instead.*)
