open Pokemon

(** [BackPack] contains tools that [Character] can use*)
module BackPack : sig
  type backpack = {
    current_capacity : int;
    max_capacity : int;
    potions : int;
    revives : int;
  }
end

(** [Character] models the functions/characteristics of the player*)
module Character : sig
  type character = {
    character_name : string;
    pokemon_list : Pokemon.pokemon list;
    backpack : BackPack.backpack;
  }

  val init : string -> character
  (** [init name] initialize a character with named [name] with an empty Pokemon
      list and empty backpack*)

  val add_pokemon : character -> Pokemon.pokemon -> character
  (** [add_pokemon player pokemon] return character with [pokemon] added to
      pokemon list of [player]. Cannot have duplicate pokemons (with same name)
      in pokemon list, if the pokemon is already exists in the pokemon_list,
      then [add_pokemon player pokemon] returns the same list *)

  val remove_pokemon : character -> string -> character
  (** [remove_pokemon player name] return character with Pokemon of name [name]
      removed from pokemon list of [player]. If no Pokemon in list with name
      [name], return [player].*)

  val get_pokemon : character -> string -> Pokemon.pokemon
  (** [get_pokemon character pokemon_name] returns a Pokemon from the pokemon
      list of [character] by name [pokemon_name]. Raises [Not_found] if no
      Pokemon with the given name is found. *)

  val add_item : character -> string -> int -> character
  (** [add_item player item n] add [n] amount of [item] to the backpack of
      [player]. If not enough capacity to add [n], add amount that makes
      backpack reach max capacity. Raises [Failure] if an invalid item is given
      or amount is a negative number. *)

  val remove_item : character -> string -> character
  (** [remove_item player item] remove [item] from the backpack of [player].
      Raises [Failure] if there are not enough items to use or if an invalid
      item is given. *)

  val heal_all : character -> character
  (** [heal_all player] returns a character that revert the HP of all the
      pokemons in the pokemon list of [player] to its max HP, and print message
      indicating change in hp for each healed pokemon.*)
end
