(** - store players, names, ids*)

open Board
open Player

(** *)
type mode =
  | Placement_Begin
  | Placement
  | Attack

  | Attack_Won of string * string
  | Fortify of bool
  | Victory of string

type t

exception IllegalState

val init_state : board -> Player.t list -> t
(** Initializes the state of the game given the board and list of
    players*)

val current_player_turn : t -> string
(** Gets the name of the player whose turn it is*)

val current_player_cards : t -> Card.card list
(** Gets the cards owned by the current player *)

val current_mode : t -> mode
(** Gets the current mode (Placement, Attack, or Fortify)*)

val board : t -> Board.board
(** [board state] Gets the current board state *)

(* Adds new troops to the player whose turn it is based off their
   current number of territories*)
val add_new_troops : bool -> t -> t

(* Places troops in a given country and updates the state*)
val place_troops : t -> string -> int -> t

(* Returns the number of remaining troops to place for a given player *)
val remaining_troops : t -> int

(* [attack st ] *)
val attack :
  t -> Board.country_id -> Board.continent_id -> t * Attack.dice_roll

val get_attack_won_info : t -> Board.country_id * Board.country_id
(** [get_attack_won_info st] is the from and towards country of the win
    attack.

    Raises [IllegalState] if current mode is not [Attack_Won] *)

(* [attack_won] takes in a state, attacking country, defending country,
   and determines if the defending country loses all its troops*)
val attack_won : t -> int -> t

val fortify : t -> Board.country_id -> Board.country_id -> int -> t
(** [fortify st from towards troops] takes in a state, two countries and
    the amount of troops to be moved between and updates the state *)

val player_info : string -> t -> string list * string list

val country_info : string -> t -> Board.country

(* [complete_turn] moves the state to the next turn *)
val complete_turn : t -> t

val state_to_json : t -> Yojson.Basic.t

val state_from_json : Yojson.Basic.t -> t