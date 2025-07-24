open! Core

let get_best_bid_order ~(bid_order_list : Game_state.Order.t list) =
  List.filter_map bid_order_list ~f:(fun { player_id; price; _ } ->
      Option.map price ~f:(fun p -> (p, player_id)))
  |> List.max_elt ~compare:(fun (p1, _) (p2, _) -> Int.compare p1 p2)

let get_best_ask_order ~(ask_order_list : Game_state.Order.t list) =
  List.filter_map ask_order_list ~f:(fun { player_id; price; _ } ->
      Option.map price ~f:(fun p -> (p, player_id)))
  |> List.min_elt ~compare:(fun (p1, _) (p2, _) -> Int.compare p1 p2)

let update_holding_and_cash_on_trade state ~racer ~bidder ~best_ask ~asker = ()

let match_orders_for_a_racer state ~(racer : Game_state.Racer.t) ~bids ~asks =
  let racer_bids = Map.find bids racer in
  let racer_asks = Map.find asks racer in
  match (racer_bids, racer_asks) with
  | None, _ | _, None -> state
  | Some bid_order_list, Some ask_order_list -> (
      match
        (get_best_bid_order ~bid_order_list, get_best_ask_order ~ask_order_list)
      with
      | None, _ | _, None -> state
      | Some (best_bid, bidder), Some (best_ask, asker) -> (
          match best_bid >= best_ask with false -> () | true -> ()))

(* let match_orders (state : Game_state.State.t) =
  (* Match highest bid with lowest ask if bid >= ask *)
  (* Update players' holdings and cash accordingly *)
 *)
