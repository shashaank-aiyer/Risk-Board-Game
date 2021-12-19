type t

exception UnavailableTroops of int

val get_name : t -> string
(** [get_name player] is the name of the player *)

val get_available_troops : t -> int
(** [get_available_troops player] is the troops available for placement
    by this player *)

val give_troops : t -> int -> t
(** [give_troops player troops] adds troops to this player's current
    ammount available, returns updated player *)

val use_troops : t -> int -> t
(** [use_troops player troops player] uses up this player's certain
    amount of available troops, returns updated player. Raises
    [UnavailableTroops troops] if trying to use more than you have *)

val make_player : string -> t
(** [make_player name] takes in a [name] and outputs a player*)

val add_card : Card.card -> t -> t

val get_cards : t -> Card.card list

val set_cards : Card.card list -> t -> t

val player_to_json : t -> Yojson.Basic.t

val player_from_json : Yojson.Basic.t -> t