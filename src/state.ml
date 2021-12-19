(* Make this exception of string *)
open Yojson.Basic.Util

exception IllegalState

type mode =
  | Placement_Begin
  | Placement
  | Attack
  | Attack_Won of string * string
  (* Attack_Won (country from, country towards)*)
  | Fortify of bool
  (* bool represents if fortify is done or not *)
  | Victory of string
(* string represents the name of the player that won *)

type t = {
  board : Board.board;
  players : Player.t list;
  player_turn : Player.t;
  turn_mode : mode;
  received_card : bool;
  trade_bonus : int;
}

let init_state board players =
  let turn =
    match players with
    | h :: t -> h
    | [] -> raise IllegalState
  in
  {
    board;
    players;
    player_turn = turn;
    turn_mode = Placement_Begin;
    received_card = false;
    trade_bonus = 4;
  }
  [@@coverage off]

let current_player_turn (st : t) = Player.get_name st.player_turn

let current_player_cards st = Player.get_cards st.player_turn

let current_mode (st : t) = st.turn_mode

let board (st : t) = st.board

let country_info (st : t) c = Board.country st.board c

let check_mode st mode = if st.turn_mode <> mode then raise IllegalState

let rec update_players p = function
  | h :: t ->
      if Player.get_name h = Player.get_name p then p :: t
      else h :: update_players p t
  | [] -> []

let update_bonus st =
  if st.trade_bonus >= 14 then st
  else { st with trade_bonus = st.trade_bonus + 2 }

let do_trade st =
  match st.player_turn |> Player.get_cards |> Card.trade with
  | lst ->
      let p = Player.set_cards lst st.player_turn in
      ( st.trade_bonus,
        {
          (update_bonus st) with
          player_turn = p;
          players = update_players p st.players;
        } )
  | exception _ -> (0, st)

let rec get_continental_bonus
    (st : t)
    (continents : Board.continent_id list) =
  match continents with
  | [] -> 0
  | h :: t ->
      Board.get_continent_bonus st.board h + get_continental_bonus st t

(** [add_new_troops] adds troops to the player whose turn it is based
    off their number of territories *)
let add_new_troops trade (st : t) =
  check_mode st Placement_Begin;
  let trade_amount, st = if not trade then (0, st) else do_trade st in
  let give =
    let player = st.player_turn |> Player.get_name in
    let owned =
      player |> Board.get_countries_owned st.board |> List.length
    in
    (owned / 3)
    + get_continental_bonus st
        (Board.get_continents_owned st.board player)
  in
  let updated_player =
    Player.give_troops st.player_turn (if give > 3 then give else 3)
  in
  { st with player_turn = updated_player; turn_mode = Placement }

let place_troops (st : t) country troops =
  check_mode st Placement;
  if troops < 0 then raise IllegalState;
  let new_player = Player.use_troops st.player_turn troops in
  let new_board =
    Board.place_troops st.board troops country
      (Player.get_name new_player)
  in
  { st with board = new_board; player_turn = new_player }

(*Check how many troops are remaining for a player in the placement
  round*)
let remaining_troops (st : t) =
  if st.turn_mode <> Placement then raise IllegalState
  else Player.get_available_troops st.player_turn

let attack (st : t) from towards =
  check_mode st Attack;
  let outcome =
    Attack.create_attack st.board
      (Player.get_name st.player_turn)
      from towards
    |> Attack.roll_dice
  in
  let b = Board.remove_troops st.board outcome.attack_num_lost from in
  let b = Board.remove_troops b outcome.defend_num_lost towards in
  let won = 0 = Board.get_country_troops b towards in
  ( {
      st with
      board = b;
      turn_mode = (if won then Attack_Won (from, towards) else Attack);
    },
    outcome )

(* Note, the place troops method of Board should also take in the player
   name to ensure that the player owns the country *)
(* let place_troops (st : t) troops country = *)

let get_attack_won_info st =
  match st.turn_mode with
  | Attack_Won (fro, tow) -> (fro, tow)
  | _ -> raise IllegalState

let give_card p =
  let card = Card.give_card () in
  Player.add_card card p

let rec remove_player p = function
  | h :: t ->
      if Player.get_name h = p then t else h :: remove_player p t
  | [] -> []

(* check losing country owner -> check owner countries_owned -> if list
   has 1 element, remove from player list. Check player list -> if list
   has 1 element -> Return Victory State (at the end after all board
   updates) *)
let attack_won st num_troops =
  let fro, tow = get_attack_won_info st in
  let from = Board.country st.board fro in
  let towards = Board.country st.board tow in
  if towards.troops <> 0 || num_troops < 1 || from.troops <= num_troops
  then raise IllegalState
  else
    let loser = Board.get_country_owner st.board tow in
    let intermediate_board =
      Board.set_ownership st.board
        (Player.get_name st.player_turn)
        towards.name
    in
    let new_board =
      Board.move_troops intermediate_board fro tow num_troops
    in
    let p =
      if st.received_card then st.player_turn
      else give_card st.player_turn
    in
    let remaining_players =
      if List.length (Board.get_countries_owned new_board loser) = 0
      then remove_player loser st.players
      else st.players
    in
    let mode =
      if List.length remaining_players = 1 then
        Victory (Player.get_name st.player_turn)
      else Attack
    in
    {
      st with
      board = new_board;
      turn_mode = mode;
      player_turn = p;
      players = update_players p remaining_players;
      received_card = true;
    }

let rec find_next_element st players =
  match players with
  | [] -> raise IllegalState
  | [ h ] ->
      if h = st.player_turn then List.hd st.players
      else raise IllegalState
  | h :: n :: t ->
      if h = st.player_turn then n else find_next_element st (n :: t)

let rec move_to_next_player st = find_next_element st st.players

let fortify
    st
    (from : Board.country_id)
    (towards : Board.country_id)
    troops =
  check_mode st (Fortify true);
  if
    Player.get_name st.player_turn
    <> Board.get_country_owner st.board from
  then raise IllegalState
  else
    let new_board = Board.fortify_troops st.board from towards troops in
    { st with turn_mode = Fortify false; board = new_board }

let player_info name st =
  ( Board.get_countries_owned st.board name,
    Board.get_continents_owned st.board name )

let country_info name st = Board.country st.board name

let complete_turn st =
  match st.turn_mode with
  | Placement_Begin -> { st with turn_mode = Placement }
  | Placement ->
      if remaining_troops st > 0 then raise IllegalState
      else { st with turn_mode = Attack; received_card = false }
  | Attack -> { st with turn_mode = Fortify true }
  (* You should never be able to complete a turn in the middle of a
     win *)
  | Attack_Won _ -> raise IllegalState
  | Fortify b ->
      {
        st with
        turn_mode = Placement_Begin;
        player_turn = move_to_next_player st;
        received_card = false;
      }
  | Victory player -> st

let json_of_mode = function
  | Placement_Begin -> `Assoc [ ("type", `String "Placement_Begin") ]
  | Placement -> `Assoc [ ("type", `String "Placement") ]
  | Attack -> `Assoc [ ("type", `String "Attack") ]
  | Attack_Won (from, towards) ->
      `Assoc
        [
          ("type", `String "Attack_Won");
          ("from", `String from);
          ("towards", `String towards);
        ]
  | Fortify b ->
      `Assoc [ ("type", `String "Fortify"); ("complete", `Bool b) ]
  | Victory player ->
      `Assoc
        [
          ("type", `String "Placement Begin"); ("winner", `String player);
        ]
[@@coverage off]

let state_to_json st =
  `Assoc
    [
      ("turn_mode", json_of_mode st.turn_mode);
      ("current_player", Player.player_to_json st.player_turn);
      ("board", Board.board_to_json st.board);
      ( "players",
        `List (List.map (fun p -> Player.player_to_json p) st.players)
      );
      ("received_card", `Bool st.received_card);
      ("trade_bonus", `Int st.trade_bonus);
    ]
  [@@coverage off]

let mode_from_json json =
  let general_mode = json |> member "type" |> to_string in
  match general_mode with
  | "Placement_Begin" -> Placement_Begin
  | "Placement" -> Placement
  | "Attack" -> Attack
  | "Attack_Won" ->
      Attack_Won
        ( json |> member "from" |> to_string,
          json |> member "towards" |> to_string )
  | "Fortify" -> Fortify (json |> member "complete" |> to_bool)
  | "Victory" -> Victory (json |> member "winner" |> to_string)
  | _ -> Placement_Begin

let state_from_json json =
  {
    board = json |> member "board" |> Board.from_json;
    players =
      json |> member "players" |> to_list
      |> List.map Player.player_from_json;
    player_turn =
      json |> member "current_player" |> Player.player_from_json;
    received_card = json |> member "received_card" |> to_bool;
    trade_bonus = json |> member "trade_bonus" |> to_int;
    turn_mode = json |> member "turn_mode" |> mode_from_json;
  }
