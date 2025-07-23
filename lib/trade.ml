open! Core

let get_best_bid_order ~(bid_order_list : Game_state.Order.t list) =
  List.filter_map bid_order_list ~f:(fun { player_id; price; _ } ->
      Option.map price ~f:(fun p -> (p, player_id)))
  |> List.max_elt ~compare:(fun (p1, _) (p2, _) -> Int.compare p1 p2)

let get_best_ask_order ~(ask_order_list : Game_state.Order.t list) =
  List.filter_map ask_order_list ~f:(fun { player_id; price; _ } ->
      Option.map price ~f:(fun p -> (p, player_id)))
  |> List.min_elt ~compare:(fun (p1, _) (p2, _) -> Int.compare p1 p2)

let match_for_racer ~(racer : Game_state.Racer.t) ~bids ~asks =
  let (racer_bids : Game_state.Order.t list option) = Map.find bids racer in
  let (racer_asks : Game_state.Order.t list option) = Map.find asks racer in
  match (racer_bids, racer_asks) with
  | None, _ | _, None -> ()
  | Some bid_order_list, Some ask_order_list -> (
      match
        (get_best_bid_order ~bid_order_list, get_best_ask_order ~ask_order_list)
      with
      | None, _ | _, None -> ()
      | Some best_bid_order, Some best_ask_order -> ())

(* let match_orders (state : Game_state.State.t) =
  (* Match highest bid with lowest ask if bid >= ask *)
  (* Update players' holdings and cash accordingly *)
 *)
