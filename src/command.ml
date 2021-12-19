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

exception WrongAction

let rec convert_to_space x = if x = '_' then ' ' else x

let handle_phrase phrase = String.map convert_to_space phrase

let parse str =
  str |> String.trim
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> " ")
  |> function
  | [] -> raise Empty (* tell player to input / say nothing is there *)
  | [ "" ] ->
      raise Empty (* tell player to input / say nothing is there *)
  | "Player" :: "Amount" :: num :: _ -> Num_Players (int_of_string num)
  | "Add" :: phrase :: _ -> Add (handle_phrase phrase)
  | "Player" :: "Info" :: phrase :: _ ->
      Player_Info (handle_phrase phrase)
  | "Country" :: "Info" :: phrase :: _ ->
      Country_Info (handle_phrase phrase)
  | "Pick" :: phrase :: _ -> Pick (handle_phrase phrase)
  | "Attack" :: phrase :: phrase2 :: _ ->
      Attack (handle_phrase phrase, handle_phrase phrase2)
  | "Fortify" :: from :: towards :: troops :: _ ->
      Fortify
        (handle_phrase from, handle_phrase towards, int_of_string troops)
  | "Place" :: num :: phrase :: _ ->
      Place (int_of_string num, handle_phrase phrase)
  | "Trade" :: _ -> Trade
  | "Move" :: num :: _ -> Move (int_of_string num)
  | "Continue" :: _ -> Continue
  | "Exit" :: _ -> Exit
  | _ -> raise Malformed
