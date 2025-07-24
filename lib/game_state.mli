module Racer : sig
  type t = Red | Yellow | Blue | Green [@@deriving compare, hash, sexp_of]

  module Map : Map.S with type key = t
  (** A map keyed by [Racer.t] values. *)
end

type position = int (* lap position or index *)
type velocity = int
type holding = { racer : Racer.t; quantity : int }
type order_type = Bid | Ask

module Order : sig
  type t = {
    player_id : string;
    racer : Racer.t;
    price : int option;
    order_type : order_type;
  }
end

module Player : sig
  type t = { id : string; holdings : Racer.t list; cash : int }

  val create : string -> Racer.t list -> t
end

module State : sig
  type t = {
    players : player list;
    bids : Order.t list Racer.Map.t;
    asks : Order.t list Racer.Map.t;
    race_positions : (Racer.t * position * velocity) list;
    is_game_over : bool;
  }

  val create :
    players:Player.t list ->
    bids:Order.t list Racer.Map.t ->
    asks:Order.t list Racer.Map.t ->
    race_positions:(Racer.t * velocity * velocity) list ->
    is_game_over:bool ->
    t

  val update :
    players:Player.t list ->
    bids:Order.t list Racer.Map.t ->
    asks:Order.t list Racer.Map.t ->
    race_positions:(Racer.t * velocity * velocity) list ->
    is_game_over:bool ->
    t

  val update_positions : t -> t
  val update_velocities : t -> t
end
