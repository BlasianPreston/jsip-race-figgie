open! Core
open Race_figgie
open Game_state

let rec game_loop state =
  (* Display race positions *)
  List.iter
    ~f:(fun (racer, pos, vel) ->
      Printf.printf
        "%s is at position %d with velocity %d\n"
        (match racer with
         | Racer.Red -> "Red"
         | Racer.Yellow -> "Yellow"
         | Racer.Blue -> "Blue"
         | Racer.Green -> "Green")
        pos
        vel)
    state.State.race_positions;
  (* Display player holdings and cash *)
  List.iter
    ~f:(fun p ->
      let holdings_str =
        String.concat
          ~sep:", "
          (List.map
             ~f:(fun r ->
               match r with
               | Racer.Red -> "Red"
               | Racer.Yellow -> "Yellow"
               | Racer.Blue -> "Blue"
               | Racer.Green -> "Green")
             p.Player.holdings)
      in
      Printf.printf
        "Player %s: $%d holdings: %s\n"
        p.Player.id
        p.Player.cash
        holdings_str)
    state.State.players;
  (* TODO: Prompt for orders here and build updated bids/asks *)

  (* Update state: process race movement *)
  let state = state |> State.update_velocities |> State.update_positions in
  (* Check for winner *)
  match state.State.winner with
  | Some r ->
    Printf.printf
      "Race over! Winner is %s\n"
      (match r with
       | Racer.Red -> "Red"
       | Racer.Yellow -> "Yellow"
       | Racer.Blue -> "Blue"
       | Racer.Green -> "Green")
  | None -> game_loop state
;;

let () =
  (* Example initialization *)
  let players =
    [ Player.create "Player1" []
    ; Player.create "Player2" []
    ; Player.create "Player3" []
    ; Player.create "You" []
    ]
  in
  let bids = Racer.Map.empty in
  let asks = Racer.Map.empty in
  let race_positions =
    [ Racer.Red, 0, 0
    ; Racer.Yellow, 0, 0
    ; Racer.Blue, 0, 0
    ; Racer.Green, 0, 0
    ]
  in
  let initial_state =
    State.create ~players ~bids ~asks ~race_positions ~winner:None
  in
  game_loop initial_state
;;
