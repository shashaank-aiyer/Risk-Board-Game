(** - Parse string inputs *)

type t =
  | Add of string
  | Player_Info of string
  | Country_Info of string
  | Pick of string
  | Attack of string * string
  | Fortify of string * string * int
  | Place of int * string
  | Move of int
  | Continue
  | Num_Players of int
  | Trade
  | Exit

exception Empty

exception Malformed

val parse : string -> t
(** [parse str] takes an input [str] and returns the corresponding
    action to be taken *)
