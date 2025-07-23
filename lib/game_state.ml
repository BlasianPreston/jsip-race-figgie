module Racer = struct
  type t = Red | Yellow | Blue | Green [@@deriving compare, hash, sexp_of]

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

type position = int (* lap position or index *)
type velocity = int
type holding = { racer : Racer.t; quantity : int }
type player = { id : string; holdings : holding list; cash : int }
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
    players : player list;
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

  let create ~players ~bids ~asks ~race_positions ~is_game_over =
    { players; bids; asks; race_positions; is_game_over }

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
