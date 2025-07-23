type racer = Red | Yellow | Blue | Green
type position = int (* lap position or index *)
type velocity = int
type holding = { racer : racer; quantity : int }
type player = { id : string; holdings : holding list; cash : int }
type order_type = Bid | Ask

module Order = struct
  type t = {
    player_id : string;
    racer : racer;
    price : int option;
    order_type : order_type;
  }

  let create ~player_id ~racer ~price ~order_type =
    { player_id; racer; price; order_type }

  let is_no_order t = match t.price with None -> true | Some _ -> false
end

type trade = { buyer : string; seller : string; racer : racer; price : int }

module State = struct
  type t = {
    players : player list;
    orders : Order.t list;
    race_positions : (racer * position * velocity) list;
    is_game_over : bool;
  }

  let empty = {players = []; orders = []; race_positions = []; is_game_over = false}

  let create ~players ~orders ~race_positions ~is_game_over = {players; orders; race_positions; is_game_over}

  let update_positions t =
    let positions = t.race_positions in
    let race_positions = List.map
      (function
        | racer, (position : position), velocity ->
            (racer, position + velocity, velocity))
      positions in
    {players=t.players; orders=t.orders; race_positions; is_game_over=t.is_game_over}
    

  let updated_velocities t =
    let positions = t.race_positions in
    let race_positions = List.map
      (function
        | racer, (position : position), _ ->
            let () = Random.self_init () in
            let magnitude = 1 + Random.int 10 in
            let sign = if Random.int 10 = 0 then -1 else 1 in
            let new_velocity = sign * magnitude in
            (racer, position, new_velocity))
      positions in
      {players=t.players; orders=t.orders; race_positions; is_game_over=t.is_game_over}
end
