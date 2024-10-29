val is_stop : int -> int -> bool
val move_all : int -> Aircraft.t list -> int ref -> unit

val get_available_speed_box :
  Geom.t -> (Geom.t * Geom.t * bool) list -> Geom.t list

val get_available_speed : Geom.t -> Geom.t -> Geom.t list -> Geom.t