open! Core

let remove_one_racer_from_list list racer =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t ->
      if Game_state.Racer.equal h racer
      then List.rev_append acc t
      else aux (h :: acc) t
  in
  aux [] list
;;

let get_best_bid_order ~(bid_order_list : Game_state.Order.t list) =
  List.filter_map bid_order_list ~f:(fun { player_id; price; _ } ->
    Option.map price ~f:(fun p -> p, player_id))
  |> List.max_elt ~compare:(fun (p1, _) (p2, _) -> Int.compare p1 p2)
;;

let get_best_ask_order ~(ask_order_list : Game_state.Order.t list) =
  List.filter_map ask_order_list ~f:(fun { player_id; price; _ } ->
    Option.map price ~f:(fun p -> p, player_id))
  |> List.min_elt ~compare:(fun (p1, _) (p2, _) -> Int.compare p1 p2)
;;

let rec remove_player_order_from_order_list
          ~player
          ~(order_list : Game_state.Order.t list)
  =
  match order_list with
  | [] -> []
  | current_order :: rest ->
    (match String.equal current_order.player_id player with
     | true -> remove_player_order_from_order_list ~player ~order_list:rest
     | false ->
       [ current_order ]
       @ remove_player_order_from_order_list ~player ~order_list:rest)
;;

let update_state_on_trade
      (state : Game_state.State.t)
      ~bidder
      ~trade_price
      ~bid
      ~asker
      ~ask_order_list
      ~bid_order_list
      ~(racer_traded : Game_state.Racer.t)
  =
  let updated_bid_order_list =
    remove_player_order_from_order_list
      ~player:bidder
      ~order_list:bid_order_list
  in
  let updated_ask_order_list =
    remove_player_order_from_order_list
      ~player:asker
      ~order_list:ask_order_list
  in
  let askers_player_record =
    List.find_exn state.players ~f:(fun (player : Game_state.Player.t) ->
      String.equal player.id asker)
  in
  let updated_asker =
    { askers_player_record with
      cash = askers_player_record.cash + trade_price
    ; holdings =
        remove_one_racer_from_list askers_player_record.holdings racer_traded
    }
  in
  let bidders_player_record =
    List.find_exn state.players ~f:(fun (player : Game_state.Player.t) ->
      String.equal player.id bidder)
  in
  let updated_bidder =
    { bidders_player_record with
      cash = bidders_player_record.cash + (bid - trade_price)
    ; holdings = racer_traded :: bidders_player_record.holdings
    }
  in
  let updated_players =
    List.map state.players ~f:(fun p ->
      if String.equal p.id asker
      then updated_asker
      else if String.equal p.id bidder
      then updated_bidder
      else p)
  in
  (* Update the bids map *)
  let updated_bids =
    Game_state.Racer.Map.add racer_traded updated_bid_order_list state.bids
  in
  (* Update the asks map *)
  let updated_asks =
    Game_state.Racer.Map.add racer_traded updated_ask_order_list state.asks
  in
  Game_state.State.update
    ~players:updated_players
    ~bids:updated_bids
    ~asks:updated_asks
    ~race_positions:state.race_positions
    ~is_game_over:state.is_game_over
;;

let check_for_trades_given_racer
      (state : Game_state.State.t)
      ~(racer : Game_state.Racer.t)
  =
  let bids = state.bids in
  let asks = state.asks in
  let racer_bids = Game_state.Racer.Map.find_opt racer bids in
  let racer_asks = Game_state.Racer.Map.find_opt racer asks in
  match racer_bids, racer_asks with
  | None, _ | _, None -> state
  | Some bid_order_list, Some ask_order_list ->
    (match
       get_best_bid_order ~bid_order_list, get_best_ask_order ~ask_order_list
     with
     | None, _ | _, None -> state
     | Some (best_bid, bidder), Some (best_ask, asker) ->
       (match best_bid >= best_ask with
        | false -> state
        | true ->
          update_state_on_trade
            state
            ~bidder
            ~trade_price:best_ask
            ~bid:best_bid
            ~asker
            ~ask_order_list
            ~bid_order_list
            ~racer_traded:racer))
;;

let match_orders (state : Game_state.State.t) =
  (* Match highest bid with lowest ask if bid >= ask *)
  (* Update players' holdings and cash accordingly *)
  check_for_trades_given_racer state ~racer:Red
  |> check_for_trades_given_racer ~racer:Green
  |> check_for_trades_given_racer ~racer:Yellow
  |> check_for_trades_given_racer ~racer:Blue
;;
