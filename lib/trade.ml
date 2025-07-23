let match_orders (state : Game_state.State.t) =
  (* Match highest bid with lowest ask if bid >= ask *)
  (* Update players' holdings and cash accordingly *)
  let orders = state.orders in
  
