(** - Move from location to location
    - add/remove troops from location
    - Change ownership
    - attack (2 locations parameters, attackers, return dice roll *)

(* type board *)

type country_id = string

type continent_id = string

(* FOR TESTING ONLY but country will be public *)
type country = {
  name : country_id;
  continent : continent_id;
  adjacent_countries : country_id list;
  troops : int;
  owner : string;
}

type continent = {
  name : continent_id;
  countries : country_id list;
  bonus : int;
}

type board = {
  continents : continent list;
  countries : country list;
}

exception MissingLocation of country_id

exception UnallowedMovement of country_id

exception UnallowedMovementAmount of int

exception MissingInfo

val empty : board

(* type country This should be in implementation*)

val from_json : Yojson.Basic.t -> board
(** [from_json json] returns a board from [json] containing JSON
    representation of board *)

val set_up : board -> (country_id * string * int) list -> board
(** [set_up board info ] returns a board where each country now belongs
    to the player given by info and has number of troops given by info.

    Raises [MissingInfo] if not all countries are in info *)

val country : board -> country_id -> country
(** [country board country_id] returns the country in [board] with id
    [country_id].

    Raises [MissingLocation country_id] if the country [country_id] does
    not exist in [board] *)

val countries : board -> country_id list
(** [countries board] returns a list of all countries on board. *)

val continents : board -> continent_id list
(** [continents board] returns a list of all continents on board *)

val countries_in_continent : board -> continent_id -> country_id list
(** [countries_in_continent board continent] returns a list of the
    countries in continent [continent] in [board].

    Raises [MissingLocation continent] if the continent [continent] does
    not exist in [board] *)

val adjacent_countries : board -> country_id -> country_id list
(** [adjacent_countries board country] returns the list of adjacent
    countries to country [country] in [board].

    Raises [MissingLocation country] if the country [country] does not
    exist in [board] *)

val move_troops : board -> country_id -> country_id -> int -> board
(**[move_troops board from towards amount] returns an updated board with
   [amount] of troops moved from [from] to [towards].

   Raises [UnallowedMovementAmount amount] if attempting to move all
   troops of [from].

   Raises [UnallowedMovement towards] if [towards] is not adjacent to
   [from] or if [towards] does not belong to the same owner as [from] *)

val place_troops : board -> int -> country_id -> string -> board
(** [place_troops board amount country player] returns an updated board
    with [amount] of troops placed at [country].

    Raises [UnallowedMovement c] if [player] is not owner of teh country *)

(*Maybe this method below should not be public? *)

val remove_troops : board -> int -> country_id -> board
(** [remove_troops board amount country] returns an updated board with
    [amount] of troops removed from [country]

    Raises [UnallowedMovementAmount num] if [amount] is greater than
    number of troops current at [country] *)

val fortify_troops : board -> country_id -> country_id -> int -> board

val set_ownership : board -> string -> country_id -> board
(** [set_ownership board player country] returns an updated board with
    [country] now belonging to [player] *)

val get_countries_owned : board -> string -> country_id list
(** [get_countries_owned board player] returns a list of country ids
    belonging to [player]. Returns the empty list if no countries belong
    to [player] *)

val get_continents_owned : board -> string -> continent_id list
(** [get_continents_owned board player] returns a list of continent ids
    belonging to [player]. Returns the empty list if no continents
    belong to [player] *)

val get_continent_bonus : board -> continent_id -> int
(** [get_continent_bonus board continent] returns the bonus troops
    allocated for [continent].

    Raises [MissingLocation continent] if [continent] does not exist in
    [board] *)

val get_country_troops : board -> country_id -> int
(** [get_country_troops board country] returns the number of troops in
    [country].

    Raises [MissingLocation country] if [country] does not exist in
    [board] *)

val get_country_owner : board -> country_id -> string
(** [get_country_owner board country] returns the player owner of
    [country]. Returns empty string if country does not have owner.

    Raises [MissingLocation country] if [country] does not exist in
    [board] *)

val board_to_json : board -> Yojson.Basic.t
(** [board_to_json board] returns a json file of the current [board] *)