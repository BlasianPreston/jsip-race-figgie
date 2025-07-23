type racer = Red | Yellow | Blue | Green
type position = int (* lap position or index *)
type holding = { racer : racer; quantity : int }
type player = { id : string; holdings : holding list; cash : int }
type order_type = Bid | Ask

type order = {
  player_id : string;
  racer : racer;
  price : int;
  quantity : int;
  order_type : order_type;
}

type trade = { buyer : string; seller : string; racer : racer; price : int }

type game_state = {
  players : player list;
  orders : order list;
  race_positions : (racer * position) list;
  laps_left : int;
}
