val outc : out_channel
val output_routes : Aircraft.t list -> unit
val output_obstacle : unit
val output : Aircraft.t list -> Geom.t list array -> int -> unit
(* val output_memory : Aircraft.t list -> unit  *)
(* val plot_to_screen : unit *)