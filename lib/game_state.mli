type racer = Red | Yellow | Blue | Green
type position = int (* lap position or index *)
type velocity = int
type holding = { racer : racer; quantity : int }
type player = { id : string; holdings : holding list; cash : int }
type order_type = Bid | Ask

module Order : sig
  type t = {
    player_id : string;
    racer : racer;
    price : int option;
    order_type : order_type;
  }
end

module State : sig
  type t = {
    players : player list;
    orders : Order.t list;
    race_positions : (racer * position * velocity) list;
    is_game_over : bool;
  }
end
