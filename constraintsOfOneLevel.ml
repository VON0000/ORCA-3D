open Aircraft

let pi = acos (-1.)

(* 边界里为非扇形区域 此方程包含对边界外的处理 *)
let get_smallest_change_to_edge_for_non_sectoral_area relative_speed
    angle_edge_right angle_edge_left is_right =
  let angle_entre_relative_speed_et_edge, direction_smallest_change =
    let angle_relative_speed = Geom.angle_2d relative_speed in
    if is_right then
      let angle_entre_relative_speed_et_right_edge =
        angle_relative_speed -. angle_edge_right
      in
      ( angle_entre_relative_speed_et_right_edge,
        Geom.create_t
          (cos (angle_edge_right -. (pi /. 2.)) /. 2.)
          (sin (angle_edge_right -. (pi /. 2.)) /. 2.) )
    else
      let angle_entre_relative_speed_et_left_edge =
        angle_edge_left -. angle_relative_speed
      in
      ( angle_entre_relative_speed_et_left_edge,
        Geom.create_t
          (cos (angle_edge_left +. (pi /. 2.)) /. 2.)
          (sin (angle_edge_left +. (pi /. 2.)) /. 2.) )
  in

  let norm_smallest_change =
    let norm_relative_speed = Geom.norm_2d relative_speed in
    norm_relative_speed *. sin angle_entre_relative_speed_et_edge
  in
  let abs_norm_smallest_change = abs_float norm_smallest_change in

  ( Geom.resize_2d direction_smallest_change (1. /. norm_smallest_change),
    Geom.resize_2d direction_smallest_change (1. /. abs_norm_smallest_change) )

let get_smallest_change_to_edge_for_sectoral_area relative_speed
    centre_of_small_circle tau =
  let vecteur_de_centre_of_small_circle_a_relative_speed =
    Geom.diff_2d relative_speed centre_of_small_circle
  in
  let norm_vecteur_de_centre_of_small_circle_a_relative_speed =
    Geom.norm_2d vecteur_de_centre_of_small_circle_a_relative_speed
  in
  let distance_to_edge =
    (2. *. Const.norme /. tau)
    -. norm_vecteur_de_centre_of_small_circle_a_relative_speed
  in
  let direction_smallest_change =
    Geom.resize_2d vecteur_de_centre_of_small_circle_a_relative_speed
      (1.
      /. (distance_to_edge
        /. norm_vecteur_de_centre_of_small_circle_a_relative_speed /. 2.))
  in
  if distance_to_edge >= 0. then
    (direction_smallest_change, direction_smallest_change)
  else (direction_smallest_change, Geom.opp_2d direction_smallest_change)

let get_smallest_change_to_edge local_acft ref_acft tau =
  (* local_acft to ref_acft *)
  let centre_of_large_circle =
    Geom.diff_2d ref_acft.position local_acft.position
  in
  let norm_centre_of_large_circle = Geom.norm_2d centre_of_large_circle
  and angle_centre_of_large_circle = Geom.angle_2d centre_of_large_circle in

  let centre_of_small_circle = Geom.resize_2d centre_of_large_circle tau in
  let norm_centre_of_small_circle = Geom.norm_2d centre_of_small_circle in

  let relative_speed = Geom.diff_2d local_acft.speed ref_acft.speed in

  if norm_centre_of_large_circle < 2. *. Const.norme then failwith "npr<2*norme"
  else
    let half_angle_between_two_edge =
      asin (2. *. Const.norme /. norm_centre_of_large_circle)
    in
    let angle_edge_right =
      angle_centre_of_large_circle -. half_angle_between_two_edge
    and angle_edge_left =
      angle_centre_of_large_circle +. half_angle_between_two_edge
    in
    let projecton_point_right =
      Geom.create_t
        (norm_centre_of_small_circle
        *. cos half_angle_between_two_edge
        *. cos angle_edge_right)
        (norm_centre_of_small_circle
        *. cos half_angle_between_two_edge
        *. sin angle_edge_right)
    and projecton_point_left =
      Geom.create_t
        (norm_centre_of_small_circle
        *. cos half_angle_between_two_edge
        *. cos angle_edge_left)
        (norm_centre_of_small_circle
        *. cos half_angle_between_two_edge
        *. sin angle_edge_left)
    in

    if
      Geom.vectoriel_three_point_2d projecton_point_right centre_of_small_circle
        relative_speed
      > 0.
      && Geom.vectoriel_three_point_2d centre_of_large_circle
           centre_of_small_circle relative_speed
         <= 0.
    then
      Some
        (get_smallest_change_to_edge_for_non_sectoral_area relative_speed
           angle_edge_right angle_edge_left true)
    else if
      Geom.vectoriel_three_point_2d centre_of_large_circle
        centre_of_small_circle relative_speed
      > 0.
      && Geom.vectoriel_three_point_2d projecton_point_left
           centre_of_small_circle relative_speed
         <= 0.
    then
      Some
        (get_smallest_change_to_edge_for_non_sectoral_area relative_speed
           angle_edge_right angle_edge_left false)
    else
      Some
        (get_smallest_change_to_edge_for_sectoral_area relative_speed
           centre_of_small_circle tau)

let get_constraints_entre_avions i acfts constraints tau =
  let local_acft = List.nth acfts i in
  for j = 0 to i - 1 do
    let ref_acft = List.nth acfts j in
    if ref_acft.active && ref_acft.level == local_acft.level then
      match get_smallest_change_to_edge local_acft ref_acft tau with
      | Some (vecteur_to_edge, positive_direction_of_vecteur) ->
          if Geom.scal_2d vecteur_to_edge vecteur_to_edge > Const.epsilon then (
            constraints.(i) <-
              (vecteur_to_edge, positive_direction_of_vecteur, true)
              :: constraints.(i);
            constraints.(j) <-
              ( Geom.opp_2d vecteur_to_edge,
                Geom.opp_2d positive_direction_of_vecteur,
                true )
              :: constraints.(j))
      | None -> ()
  done

(* 首先寻找从无人机当前位置指向障碍物最左边或最右边的点的向量
   以previ到这个向量的垂线为需要避免的速度大小（只关注大小）
   选取左右两边较小的一边返回
   此时返回的速度指向障碍物边缘点与当前位置的外侧 *)
let get_delta_speed previ local_acft d g =
  let delta_v_droite =
    Geom.projection_point_to_vector previ local_acft.position
      (Geom.diff_2d d local_acft.position)
  in
  let angle_ext_position_d =
    Geom.angle_2d (Geom.diff_2d d local_acft.position)
  in
  let angle_ext_d =
    Geom.create_t
      (cos (angle_ext_position_d -. (Geom.pi /. 2.)))
      (sin (angle_ext_position_d -. (Geom.pi /. 2.)))
  in
  let delta_v_gauche =
    Geom.projection_point_to_vector previ local_acft.position
      (Geom.diff_2d g local_acft.position)
  in
  let angle_ext_position_g =
    Geom.angle_2d (Geom.diff_2d g local_acft.position)
  in
  let angle_ext_g =
    Geom.create_t
      (cos (angle_ext_position_g +. (Geom.pi /. 2.)))
      (sin (angle_ext_position_g +. (Geom.pi /. 2.)))
  in
  if
    Geom.scal_2d delta_v_droite delta_v_droite
    < Geom.scal_2d delta_v_gauche delta_v_gauche
  then (delta_v_droite, angle_ext_d)
  else (delta_v_gauche, angle_ext_g)

let get_constraints_obstacle i acfts constraints tau =
  let local_acft = List.nth acfts i in
  let previ_opt_tau =
    Geom.create_t
      (local_acft.position.x +. (tau *. local_acft.speedopt.x))
      (local_acft.position.y +. (tau *. local_acft.speedopt.y))
  and previ =
    Geom.create_t
      (local_acft.position.x +. (Const.pas *. local_acft.speed.x))
      (local_acft.position.y +. (Const.pas *. local_acft.speed.y))
  and previ_tau =
    Geom.create_t
      (local_acft.position.x +. (tau *. local_acft.speed.x))
      (local_acft.position.y +. (tau *. local_acft.speed.y))
  in
  for j = 0 to Array.length Env.obstacle - 1 do
    if
      Geom.cross_segconv local_acft.position previ_opt_tau Env.obstacle.(j)
      || Geom.cross_segconv local_acft.position previ_tau Env.obstacle.(j)
    then
      let d, g = Geom.extremes local_acft.position Env.obstacle.(j) in
      let delta_v, angle_ext = get_delta_speed previ local_acft d g in
      constraints.(i) <- (delta_v, angle_ext, false) :: constraints.(i)
  done
