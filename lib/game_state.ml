module Racer = struct
  type t = Red | Yellow | Blue | Green [@@deriving compare, hash, sexp_of]

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

type position = int
type velocity = int
type holding = { racer : Racer.t; quantity : int }

module Player = struct
  type t = { id : string; holdings : Racer.t list; cash : int }

  let create name hand = { id = name; holdings = hand; cash = 400 }
end

type order_type = Bid | Ask

module Order = struct
  type t = {
    player_id : string;
    racer : Racer.t;
    price : int option;
    order_type : order_type;
  }

  let create ~player_id ~racer ~price ~order_type =
    { player_id; racer; price; order_type }

  let is_no_order t = match t.price with None -> true | Some _ -> false
end

type trade = { buyer : string; seller : string; racer : Racer.t; price : int }

module State = struct
  type t = {
    players : Player.t list;
    bids : Order.t list Racer.Map.t;
    asks : Order.t list Racer.Map.t;
    race_positions : (Racer.t * position * velocity) list;
    is_game_over : bool;
  }

  let empty =
    {
      players = [];
      bids = Racer.Map.empty;
      asks = Racer.Map.empty;
      race_positions = [];
      is_game_over = false;
    }

  let rec shuffle = function
    | [] -> []
    | [ single ] -> [ single ]
    | list ->
        let before, after = List.partition (fun _ -> Random.bool ()) list in
        List.rev_append (shuffle before) (shuffle after)

  let distribute lst (n : int) =
    let chunk_size = List.length lst / n in
    let rec split acc current lst count =
      match (lst, count) with
      | [], _ -> List.rev (List.rev current :: acc)
      | x :: xs, 1 -> split (List.rev (x :: current) :: acc) [] xs chunk_size
      | x :: xs, _ -> split acc (x :: current) xs (count - 1)
    in
    split [] [] lst chunk_size

  let add_hands_to_players t =
    let players = t.players in
    let deck =
      List.init 10 (fun _ -> Racer.Red)
      @ List.init 10 (fun _ -> Racer.Blue)
      @ List.init 10 (fun _ -> Racer.Green)
      @ List.init 10 (fun _ -> Racer.Yellow)
    in
    let shuffled_deck = shuffle deck in
    let groups = distribute shuffled_deck 4 in
    let players_with_cards =
      List.map2
        (fun (player : Player.t) cards -> { player with holdings = cards })
        players groups
    in
    {
      players = players_with_cards;
      bids = t.bids;
      asks = t.asks;
      race_positions = t.race_positions;
      is_game_over = false;
    }

  let create ~players ~bids ~asks ~race_positions ~is_game_over =
    let state = { players; bids; asks; race_positions; is_game_over } in
    add_hands_to_players state

  let update_positions t =
    let positions = t.race_positions in
    let race_positions =
      List.map
        (function
          | racer, (position : position), velocity ->
              (racer, position + velocity, velocity))
        positions
    in
    {
      players = t.players;
      bids = t.bids;
      asks = t.asks;
      race_positions;
      is_game_over = t.is_game_over;
    }

  let updated_velocities t =
    let positions = t.race_positions in
    let race_positions =
      List.map
        (function
          | racer, (position : position), _ ->
              let () = Random.self_init () in
              let magnitude = 1 + Random.int 10 in
              let sign = if Random.int 10 = 0 then -1 else 1 in
              let new_velocity = sign * magnitude in
              (racer, position, new_velocity))
        positions
    in
    {
      players = t.players;
      bids = t.bids;
      asks = t.asks;
      race_positions;
      is_game_over = t.is_game_over;
    }
end
