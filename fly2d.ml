(* lib/fly2d.ml *)
open Aircraft
open Geom
open Const
open Plot

let sizelong = 1. *. pas (*manoeuvrabilité longitudinale*)
let fichmem = open_out "./results/memory"
let is_stop dim fin = if fin < dim then true else false

(*mesure de la distance totale*)
let totdist = ref 0.

let move_all dim acfts flag_fin =
  for i = 0 to dim - 1 do
    let acft = List.nth acfts i in
    if acft.active then (
      Printf.fprintf fichmem "%f %f\n" acft.position.x acft.position.y;
      totdist := !totdist +. Geom.norm_2d acft.speed;
      Aircraft.move_one acft;
      if Geom.dist2_2d acft.position acft.dest < 4. *. Const.norme2 then (
        acft.active <- false;
        (* incr 用于递增一个引用类型的整数值 *)
        incr flag_fin);
      Printf.fprintf fichmem "%f %f\n\n" acft.position.x acft.position.y;
      flush fichmem)
  done

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
    centre_of_small_circle =
  let vecteur_de_centre_of_small_circle_a_relative_speed =
    Geom.diff_2d relative_speed centre_of_small_circle
  in
  let norm_vecteur_de_centre_of_small_circle_a_relative_speed =
    Geom.norm_2d vecteur_de_centre_of_small_circle_a_relative_speed
  in
  let distance_to_edge =
    (2. *. Const.norme /. Const.tau)
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

let get_smallest_change_to_edge local_acft ref_acft =
  (* local_acft to ref_acft *)
  let centre_of_large_circle =
    Geom.diff_2d ref_acft.position local_acft.position
  in
  let norm_centre_of_large_circle = Geom.norm_2d centre_of_large_circle
  and angle_centre_of_large_circle = Geom.angle_2d centre_of_large_circle in

  let centre_of_small_circle =
    Geom.resize_2d centre_of_large_circle Const.tau
  in
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
           centre_of_small_circle)

let get_constraints_entre_avions i acfts constraints =
  let local_acft = List.nth acfts i in
  for j = 0 to i - 1 do
    let ref_acft = List.nth acfts j in
    if ref_acft.active then
      match get_smallest_change_to_edge local_acft ref_acft with
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

let get_constraints_obstacle i acfts constraints =
  let local_acft = List.nth acfts i in
  let previ_opt_tau =
    Geom.create_t
      (local_acft.position.x +. (Const.tau *. local_acft.speedopt.x))
      (local_acft.position.y +. (Const.tau *. local_acft.speedopt.y))
  and previ =
    Geom.create_t
      (local_acft.position.x +. (Const.pas *. local_acft.speed.x))
      (local_acft.position.y +. (Const.pas *. local_acft.speed.y))
  and previ_tau =
    Geom.create_t
      (local_acft.position.x +. (Const.tau *. local_acft.speed.x))
      (local_acft.position.y +. (Const.tau *. local_acft.speed.y))
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

(*boite des vitesses possibles version circulaire à facettes*)
let speedbox speed =
  (*nombre de facettes de la boite contenant les vitesses*)
  let nb = 20 in
  let angle = Geom.angle_2d speed in
  let box =
    Array.init (nb + 1) (fun i ->
        let na = angle +. (float i *. 2. *. pi /. float nb) in
        { x = sizelong *. cos na; y = sizelong *. sin na })
  in
  Array.to_list box

exception Fin
exception Echec
exception Vide

let get_available_speed_box speed constraints_one_acft =
  let initbox = speedbox speed in
  let box = ref initbox in
  let delta = ref (-0.5) in
  (try
     while true do
       box := initbox;
       try
         List.iter
           (fun (vecteur_to_edge, positive_direction_of_vecteur, flag) ->
             let positive_unit_direction_of_vecteur =
               Geom.resize_2d positive_direction_of_vecteur
                 (Geom.norm_2d vecteur_to_edge)
             in
             let relaxed_limited_speed_to_edge =
               let speed_reach_the_edge =
                 Geom.diff_2d speed (Geom.opp_2d vecteur_to_edge)
               in
               if flag then
                 Geom.diff_2d speed_reach_the_edge
                   (Geom.resize_2d positive_unit_direction_of_vecteur
                      (1. /. !delta))
               else Geom.diff_2d speed_reach_the_edge Geom.default_t
             in
             box :=
               Geom.cutting_border relaxed_limited_speed_to_edge
                 positive_direction_of_vecteur !box)
           constraints_one_acft;
         raise Fin
       with Geom.Vide ->
         delta := !delta +. 0.1;
         if !delta > 20000. then raise Echec
     done
   with Fin -> ());
  !box

let get_available_speed speed speedopt speedbox =
  let newspeed =
    if Geom.is_inside speedopt speedbox then speedopt
    else Geom.projection_point_to_convex speedopt speedbox
  in
  if Geom.norm_2d newspeed > Const.const_speed /. 5. then newspeed
  else if Geom.is_inside speed speedbox then speed
  else Geom.projection_point_to_convex (Geom.resize_2d speed 1.) speedbox

let seed = truncate (Unix.time ())
let _ = Random.init seed

let () =
  let flag_fin = ref 0 in
  let time = ref 0 in
  let step = 10 (*pas de temps affichage *) in
  let acfts = Aircraft.acft_lst in

  Plot.output_routes acfts;
  Plot.output_obstacle;

  while is_stop Const.dim !flag_fin do
    let constraints = Array.init dim (fun i -> []) in
    for i = 0 to dim - 1 do
      if (List.nth acfts i).active then (
        get_constraints_entre_avions i acfts constraints;
        get_constraints_obstacle i acfts constraints)
    done;

    let boites = Array.init dim (fun i -> []) in
    for i = 0 to dim - 1 do
      let targetbox =
        get_available_speed_box (List.nth acfts i).speed constraints.(i)
      in

      let new_speed =
        get_available_speed (List.nth acfts i).speed (List.nth acfts i).speedopt
          targetbox
      in
      (List.nth acfts i).speed <- new_speed;
      boites.(i) <- targetbox
    done;
    if !time mod step = 0 then (
      Plot.output acfts boites !time;
      flush Plot.outc);
    move_all Const.dim acfts flag_fin;
    Printf.printf "\027[32m time: %d \027[0m \n" !time;
    Printf.printf "\027[32m flag_fin: %d \027[0m \n" !flag_fin;
    incr time;
    if !time > 5000 then exit 1;
  done;
  let _ = Unix.select [] [] [] 10. in
  flush Plot.outc