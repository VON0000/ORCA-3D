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

(* 得到结果为相对速度指向圆圈边界的向量 的 长度的1/2 和 方向 *)
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

let compute_circle_centers ref_pos local_pos tau =
  let large_center = Geom.diff_2d ref_pos local_pos in
  let small_center = Geom.resize_2d large_center tau in
  ( large_center,
    Geom.norm_2d large_center,
    Geom.angle_2d large_center,
    small_center,
    Geom.norm_2d small_center )

let compute_edge_angles large_norm center_angle =
  let half_angle_between_two_edge = asin (2. *. Const.norme /. large_norm) in
  let right_edge_angle = center_angle -. half_angle_between_two_edge in
  let left_edge_angle = center_angle +. half_angle_between_two_edge in
  (right_edge_angle, left_edge_angle)

let compute_projection_points small_norm half_angle_between_two_edge
    right_edge_angle left_edge_angle =
  let cos_half = cos half_angle_between_two_edge in
  (* 小圆中心到右边界的投影点 *)
  let proj_right =
    Geom.create_t
      (small_norm *. cos_half *. cos right_edge_angle)
      (small_norm *. cos_half *. sin right_edge_angle)
  (* 小圆中心到左边界的投影点 *)
  and proj_left =
    Geom.create_t
      (small_norm *. cos_half *. cos left_edge_angle)
      (small_norm *. cos_half *. sin left_edge_angle)
  in
  (proj_right, proj_left)

let get_smallest_change_to_edge local_acft ref_acft tau =
  let { position = local_pos; speed = local_speed } = local_acft in
  let { position = ref_pos; speed = ref_speed } = ref_acft in
  let large_center, large_norm, center_angle, small_center, small_norm =
    compute_circle_centers ref_pos local_pos tau
  in
  let relative_speed = Geom.diff_2d local_speed ref_speed in

  if large_norm < 2. *. Const.norme then failwith "Distance too small"
  else
    let right_edge_angle, left_edge_angle =
      compute_edge_angles large_norm center_angle
    in
    let half_angle = asin (2. *. Const.norme /. large_norm) in
    let proj_right, proj_left =
      compute_projection_points small_norm half_angle right_edge_angle
        left_edge_angle
    in

    (* > 0 relative_speed 在 proj_right small_center 连线的逆时针方向
       < 0 relative_speed 在 proj_right small_center 连线的顺时针方向 *)
    let vec_right =
      Geom.vectoriel_three_point_2d proj_right small_center relative_speed
    in
    (* > 0 relative_speed 在 proj_left small_center 连线的逆时针方向
       < 0 relative_speed 在 proj_left small_center 连线的顺时针方向 *)
    let vec_left =
      Geom.vectoriel_three_point_2d proj_left small_center relative_speed
    in
    (* > 0 relative_speed 在 large_center small_center 连线的逆时针方向
       < 0 relative_speed 在 large_center small_center 连线的顺时针方向 *)
    let vec_large_small =
      Geom.vectoriel_three_point_2d large_center small_center relative_speed
    in

    match
      ( vec_right > 0.,
        vec_large_small <= 0.,
        vec_large_small > 0.,
        vec_left <= 0. )
    with
    | true, true, _, _ ->
        Some
          (get_smallest_change_to_edge_for_non_sectoral_area relative_speed
             right_edge_angle left_edge_angle true)
    | _, _, true, true ->
        Some
          (get_smallest_change_to_edge_for_non_sectoral_area relative_speed
             right_edge_angle left_edge_angle false)
    | _ ->
        Some
          (get_smallest_change_to_edge_for_sectoral_area relative_speed
             small_center tau)

let get_constraints_entre_avions i acfts constraints tau =
  let local_acft = List.nth acfts i in
  for j = 0 to i - 1 do
    let ref_acft = List.nth acfts j in
    if ref_acft.active && ref_acft.level == local_acft.level then
      match get_smallest_change_to_edge local_acft ref_acft tau with
      | Some (vecteur_to_edge, positive_direction_of_vecteur) ->
          if Geom.norm_2d vecteur_to_edge > Const.epsilon then (
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
