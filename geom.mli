type t = { x : float; y : float }

val create_t : float -> float -> t
val default_t : t
val mean_2d : t -> t -> float * float
val diff_2d : t -> t -> t
val scal_2d : t -> t -> float
val vectoriel_2d : t -> t -> float
val scal_three_point_2d : t -> t -> t -> float
val vectoriel_three_point_2d : t -> t -> t -> float
val norm_2d : t -> float
val angle_2d : t -> float
val heading_angle : t -> t -> float
val dist2_2d : t -> t -> float
val opp_2d : t -> t
val resize_2d : t -> float -> t
val rotate : t -> float -> t
val is_inside : t -> t list -> bool
val cross_segconv : t -> t -> t list -> bool
val extremes : t -> t list -> t * t
val pi : float
val projection_point_to_vector : t -> t -> t -> t
val projection_point_to_convex : t -> t list -> t

exception Vide

val cutting_border : t -> t -> t list -> t list