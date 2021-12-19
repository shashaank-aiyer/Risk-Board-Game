type country_id = string

type continent_id = string

exception MissingLocation of country_id

exception UnallowedMovement of country_id

exception UnallowedMovementAmount of int

exception MissingInfo

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

open Yojson.Basic.Util

let empty = { continents = []; countries = [] }

(* Helper method *)
let country_from_json json =
  let num_troops = json |> member "num_troops" in
  let person = json |> member "owner" in
  {
    name = json |> member "name" |> to_string;
    continent = json |> member "continent" |> to_string;
    adjacent_countries =
      json |> member "adjacent_countries" |> to_list |> filter_string;
    troops = (if num_troops = `Null then 0 else num_troops |> to_int);
    owner = (if person = `Null then "" else person |> to_string);
  }

(* Helper method *)
let continent_from_json json =
  {
    name = json |> member "name" |> to_string;
    countries = json |> member "countries" |> to_list |> filter_string;
    bonus = json |> member "bonus" |> to_int;
  }

let from_json json =
  {
    continents =
      json |> member "continents" |> to_list
      |> List.map continent_from_json;
    countries =
      json |> member "countries" |> to_list
      |> List.map country_from_json;
  }

(* Helper method *)
let rec get_country_info info (country : country_id) =
  match info with
  | (c, name, troops) :: t ->
      if c = country then (c, name, troops)
      else get_country_info t country
  | [] -> raise MissingInfo

(* Helper method *)

let rec set_up_countries (countries : country list) info =
  match countries with
  | h :: t ->
      let country_name, player, num = get_country_info info h.name in
      { h with troops = num; owner = player } :: set_up_countries t info
  | [] -> []

let set_up (board : board) info =
  {
    continents = board.continents;
    countries = set_up_countries board.countries info;
  }

(* Helper method *)
let rec get_country_ids (countries : country list) =
  match countries with
  | [] -> []
  | h :: t -> h.name :: get_country_ids t

(* Helper method *)
let rec get_continent_ids (continents : continent list) =
  match continents with
  | [] -> []
  | h :: t -> h.name :: get_continent_ids t

(* Helper method: Looks for continent in list, returns the countries of
   the continent *)
let rec get_country_list continents continent =
  match continents with
  | [] -> raise (MissingLocation continent)
  | h :: t ->
      if h.name = continent then h.countries
      else get_country_list t continent

(* Helper method: Looks for [country_id] in a list of countries
   [countries] and returns if found *)
let rec get_country (countries : country list) country_id =
  match countries with
  | [] -> raise (MissingLocation country_id)
  | h :: t ->
      if h.name = country_id then h else get_country t country_id

let country board c = get_country board.countries c

let countries board = get_country_ids board.countries

let continents board = get_continent_ids board.continents

let countries_in_continent board continent =
  get_country_list board.continents continent

let adjacent_countries board country =
  (get_country board.countries country).adjacent_countries

(* Helper method. Checks if country is in the countries adjacent
   countries *)
let rec is_adjacent (adjacents : country_id list) (c : country) : bool =
  match adjacents with
  | h :: t -> if h = c.name then true else is_adjacent t c
  | [] -> false

(* Helper *)
let rec movement (countries : country list) from towards amount =
  match countries with
  | h :: t ->
      if h.name = from then
        { h with troops = h.troops - amount }
        :: movement t from towards amount
      else if h.name = towards then
        { h with troops = h.troops + amount }
        :: movement t from towards amount
      else h :: movement t from towards amount
  | [] -> []

let move_troops board from towards amount =
  let from_country = get_country board.countries from in
  let to_country = get_country board.countries towards in
  if from_country.troops - amount < 1 then
    raise (UnallowedMovementAmount amount)
  else if
    is_adjacent from_country.adjacent_countries to_country |> not
    || from_country.owner <> to_country.owner
  then raise (UnallowedMovement towards)
  else
    {
      board with
      countries = movement board.countries from towards amount;
    }

(* Helper method: *)
let rec update_board_countries f (countries : country list) country =
  match countries with
  | [] -> raise (UnallowedMovement country)
  | h :: t ->
      if h.name = country then f h :: t
      else h :: update_board_countries f t country

(* Helper method: *)
let update_country_troops amount player country : country =
  let stop =
    match player with
    | None -> false
    | Some p -> p <> country.owner
  in
  if stop then raise (UnallowedMovement country.name)
  else
    let new_amount = country.troops + amount in
    if new_amount < 0 then raise (UnallowedMovementAmount amount)
    else { country with troops = new_amount }

(* Helper method: *)
let update_country_owner player country : country =
  { country with owner = player }

let place_troops board amount country player =
  {
    board with
    countries =
      update_board_countries
        (update_country_troops amount (Some player))
        board.countries country;
  }

let remove_troops (board : board) (amount : int) (country : country_id)
    =
  {
    board with
    countries =
      update_board_countries
        (update_country_troops (-amount) None)
        board.countries country;
  }

(* This method currently extremely similar to move_troops, we can either
   make a helper or not because his method may get more complicated when
   we implement it properly *)
let fortify_troops board from towards amount =
  let from_country = get_country board.countries from in
  let to_country = get_country board.countries towards in
  if from_country.troops - amount < 1 || amount <= 0 then
    raise (UnallowedMovementAmount amount)
  else if from_country.owner <> to_country.owner then
    raise (UnallowedMovement towards)
  else
    {
      board with
      countries = movement board.countries from towards amount;
    }

let set_ownership board player country =
  {
    board with
    countries =
      update_board_countries
        (update_country_owner player)
        board.countries country;
  }

(* Helper *)
let rec get_countries_owned_helper player = function
  | h :: t ->
      if h.owner = player then
        h.name :: get_countries_owned_helper player t
      else get_countries_owned_helper player t
  | [] -> []

let get_countries_owned board player =
  get_countries_owned_helper player board.countries

let rec is_continent_owned player all_countries = function
  | country_id :: t ->
      let country = get_country all_countries country_id in
      if country.owner = player then
        true && is_continent_owned player all_countries t
      else false
  | [] -> true

(* Helper *)
let rec get_continents_owned_helper
    board
    player
    (continents : continent list) =
  match continents with
  | h :: t ->
      if is_continent_owned player board.countries h.countries then
        h.name :: get_continents_owned_helper board player t
      else get_continents_owned_helper board player t
  | [] -> []

let get_continents_owned board player =
  get_continents_owned_helper board player board.continents

(* Helper method: Looks for [continent_id] in a list of continents
   [continents] and returns its continent bonus *)
let rec continent_bonus (continents : continent list) continent_id =
  match continents with
  | [] -> raise (MissingLocation continent_id)
  | h :: t ->
      if h.name = continent_id then h.bonus
      else continent_bonus t continent_id

let get_continent_bonus board continent_id =
  continent_bonus board.continents continent_id

let get_country_troops board country =
  (get_country board.countries country).troops

let get_country_owner board country =
  (get_country board.countries country).owner

let country_to_json (country : country) =
  `Assoc
    [
      ("name", `String country.name);
      ("continent", `String country.continent);
      ( "adjacent_countries",
        `List (List.map (fun x -> `String x) country.adjacent_countries)
      );
      ("troops", `Int country.troops);
      ("owner", `String country.owner);
    ]

let continent_to_json continent =
  `Assoc
    [
      ("name", `String continent.name);
      ( "countries",
        `List (List.map (fun x -> `String x) continent.countries) );
      ("bonus", `Int continent.bonus);
    ]

let board_to_json board =
  `Assoc
    [
      ( "continents",
        `List (board.continents |> List.map continent_to_json) );
      ("countries", `List (board.countries |> List.map country_to_json));
    ]
