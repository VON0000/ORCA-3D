val get_constraints_entre_avions :
  int -> Aircraft.t list -> (Geom.t * Geom.t * bool) list array -> float -> unit

val get_constraints_obstacle :
  int -> Aircraft.t list -> (Geom.t * Geom.t * bool) list array -> float -> unit

