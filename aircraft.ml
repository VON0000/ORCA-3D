(* p=position; s=speed; d=dest *)
open Geom
open Env
open Const

exception Exit

type t = {
  mutable position : Geom.t;
  dest : Geom.t;
  mutable speed : Geom.t;
  mutable speedopt : Geom.t;
  route : Geom.t list;
  mutable active : bool;
}

let radius = 300. (* taille de la fenêtre *)

let judge_exist exist_list (position : Geom.t) =
  (* flag -> true 间距合理
     flag -> false 间距不合理 *)
  let flag = ref true in
  try
    for i = 0 to List.length exist_list - 1 do
      if
        Geom.dist2_2d position (List.nth exist_list i)
        < Const.sep *. Const.norme2
      then (
        flag := false;
        raise Exit)
    done;
    !flag
  with Exit -> !flag

let def_position id exist_acft =
  Random.self_init ();
  let existing_positions = List.map (fun x -> x.position) !exist_acft in
  let rec generate_position () =
    let position =
      Geom.create_t
        (if id mod 2 = 0 then radius *. (Random.float 2. -. 1.)
         else if id mod 4 = 1 then 0. -. radius
         else radius)
        (if id mod 2 = 1 then radius *. (Random.float 2. -. 1.)
         else if id mod 4 = 2 then 0. -. radius
         else radius)
    in
    if judge_exist existing_positions position then position
    else generate_position ()
  in
  generate_position ()

let def_dest id exist_acft =
  Random.self_init ();
  let existing_dests = List.map (fun x -> x.dest) !exist_acft in
  let rec generate_dest () =
    let dest =
      Geom.create_t
        (if id mod 2 = 0 then radius *. (Random.float 2. -. 1.)
         else if id mod 4 = 1 then radius
         else 0. -. radius)
        (if id mod 2 = 1 then radius *. (Random.float 2. -. 1.)
         else if id mod 4 = 2 then radius
         else 0. -. radius)
    in
    if judge_exist existing_dests dest then dest else generate_dest ()
  in
  generate_dest ()

let def_speed (position : Geom.t) (dest : Geom.t) =
  let angle = Geom.heading_angle position dest in
  Geom.create_t (Const.const_speed *. cos angle) (Const.const_speed *. sin angle)

let def_speedopt position dest =
  let cap = Geom.heading_angle position dest in
  Geom.create_t (Const.const_speed *. cos cap) (Const.const_speed *. sin cap)

let create id (exist_acft : t list ref) =
  let position = def_position id exist_acft in
  let dest = def_dest id exist_acft in
  let speed = def_speed position dest in
  let speedopt = def_speed position dest in
  let acft =
    {
      position;
      dest;
      speed;
      speedopt;
      route = [ position; dest ];
      active = true;
    }
  in
  acft

let update_speedopt acft =
  let cap = Geom.heading_angle acft.position acft.dest in
  Geom.create_t (Const.const_speed *. cos cap) (Const.const_speed *. sin cap)

let get_position acft = acft.position
let get_dest acft = acft.dest
let get_speed acft = acft.speed
let get_speedopt acft = acft.speedopt
let get_route acft = acft.route
let get_active acft = acft.active

(* 生成一个大小为 dim 的 acft 数组 *)
let get_acft_lst dim =
  let acfts = ref [] in

  (* 迭代生成每个 acft 实例，并覆盖数组中的元素 *)
  for i = 0 to dim - 1 do
    acfts := create i acfts :: !acfts
  done;

  (* 返回生成的 acft 数组 *)
  List.rev !acfts

let acft_lst = get_acft_lst Const.dim
let pas = 1. (*pas de temps*)

let move_one acft =
  let new_x = acft.position.x +. (pas *. acft.speed.x)
  and new_y = acft.position.y +. (pas *. acft.speed.y) in
  if
    not
      (Array.fold_left
         (fun tf o -> tf || Geom.is_inside (Geom.create_t new_x new_y) o)
         false Env.obstacle)
  then acft.position <- Geom.create_t new_x new_y
  else Printf.printf "ENTREE DANS OBSTACLE\n";
  acft.speedopt <- update_speedopt acft;
  ()
