(* lib/fly2d.ml *)
open Aircraft

let sizelong = 1. *. Const.pas (*manoeuvrabilité longitudinale*)
let fichmem = open_out "./results/memory"
let is_stop (dim : int) (fin : int) = if fin < dim then true else false

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

let pi = acos (-1.)

(*boite des vitesses possibles version circulaire à facettes*)
let speedbox speed =
  (*nombre de facettes de la boite contenant les vitesses*)
  let nb = 20 in
  let angle = Geom.angle_2d speed in
  let box =
    Array.init (nb + 1) (fun i ->
        let na = angle +. (float i *. 2. *. pi /. float nb) in
        Geom.create_t (sizelong *. cos na) (sizelong *. sin na))
  in
  Array.to_list box

exception Echec

let get_available_speed_box speed constraints_one_acft =
  let initbox = speedbox speed in
  let box = ref initbox in
  let delta = ref (-0.5) in
  (box := initbox;
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
               (Geom.resize_2d positive_unit_direction_of_vecteur (1. /. !delta))
           else Geom.diff_2d speed_reach_the_edge Geom.default_t
         in
         box :=
           Geom.cutting_border relaxed_limited_speed_to_edge
             positive_direction_of_vecteur !box)
       constraints_one_acft
   with Geom.Vide -> box := [ Geom.default_t ]);
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
