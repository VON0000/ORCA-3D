type t = { x : float; y : float }

let create_t x y = { x; y }
let default_t = { x = 0.; y = 0. }

(* operations de base *)
let mean_2d (a : t) (b : t) = ((a.x +. b.x) /. 2., (a.y +. b.y) /. 2.)

(* b -> a 的向量 *)
let diff_2d (a : t) (b : t) = create_t (a.x -. b.x) (a.y -. b.y)

(* 点积 *)
(* > 0 -> a b 夹角小于90度
   < 0 -> a b 夹角大于90度
   = 0 -> a b 垂直 *)
let scal_2d (a : t) (b : t) = (a.x *. b.x) +. (a.y *. b.y)

(* 叉乘 *)
(* |a| * |b| * sin(theta) theta <- a 到 b 的角度*)
(* > 0 -> b 在 a 的逆时针方向
   < 0 -> b 在 a 的顺时针方向
   = 0 -> 平行或者共线 *)
let vectoriel_2d (a : t) (b : t) = (a.x *. b.y) -. (a.y *. b.x)

(* p0 -> p1 向量 和 p0 -> p2 向量 做内积 *)
let scal_three_point_2d (p1 : t) (p0 : t) (p2 : t) =
  scal_2d (diff_2d p1 p0) (diff_2d p2 p0)

(* p0 -> p1 向量 和 p0 -> p2 向量 做外积 *)
let vectoriel_three_point_2d (p1 : t) (p0 : t) (p2 : t) =
  vectoriel_2d (diff_2d p1 p0) (diff_2d p2 p0)

(* retourne la norme et l'angle de a*)
let norm_2d (a : t) = sqrt (scal_2d a a)
let angle_2d (a : t) = atan2 a.y a.x
let heading_angle a b = atan2 (b.y -. a.y) (b.x -. a.x)

let dist2_2d (a : t) (b : t) =
  let dx = a.x -. b.x and dy = a.y -. b.y in
  (dx *. dx) +. (dy *. dy)

(* 反向 *)
let opp_2d (v : t) = create_t (-.v.x) (-.v.y)

let normal_vecteur_for_two_point_2d (a : t) (b : t) =
  { x = a.y -. b.y; y = b.x -. a.x }

let resize_2d (a : t) (num : float) = { x = a.x /. num; y = a.y /. num }

(* 将一个向量 p 绕原点旋转一个角度 a，并将其长度乘以一个系数 k *)
let rotate point angle =
  let norm_point = norm_2d point and angle_point = angle_2d point in
  create_t
    (norm_point *. cos (angle_point +. angle))
    (norm_point *. sin (angle_point +. angle))

let is_inside a l =
  match l with
  | [] -> false
  | hd :: _ ->
      let rec is_in a l =
        match l with
        | b :: (c :: _ as ctl) ->
            vectoriel_three_point_2d b c a < 0. && is_in a ctl
        | [ b ] -> vectoriel_three_point_2d b hd a <= 0.
        | _ -> failwith "is_inside: unreachable"
      in
      is_in a l

(* croisement du segment a b et c d*)
(* 判断线段ab与cd是否相交 *)
(* 判断a,b(c,d)两点是否在线段cd(ab)两侧 *)
(* True -> 相交 *)
let cross_segs a b c d =
  let ab = diff_2d b a and ac = diff_2d c a and ad = diff_2d d a in
  let cd = diff_2d d c and ca = diff_2d a c and cb = diff_2d b c in
  vectoriel_2d ab ac *. vectoriel_2d ab ad < 0.
  && vectoriel_2d cd ca *. vectoriel_2d cd cb < 0.

(* croisement du segment a b et convexe convex*)
(* 线段ab是否与凸多边形convex有相交区域 *)
(* True -> 相交 *)
let rec cross_segconv a b convex =
  (* 模式匹配 *)
  match convex with
  (* convex是一个列表，c是convex中的第一个元素，d是第二个，tl表示剩下的元素 *)
  (* d :: tl 指的是从d开始的列表切片 *)
  | c :: d :: tl -> cross_segs a b c d || cross_segconv a b (d :: tl)
  | _ -> false

(* donne les points extremes d'un convexe l vu du point a*)
(* 用来找出从一点 a 观察一个凸多边形 l 时的两个极端点。 *)
(* 这里的“极端点”指的是多边形在从点 a 的视角看时的最左边和最右边的点。 *)
let extremes a l =
  match l with
  | [] -> failwith "Erreur extremes"
  | hd :: tl ->
      List.fold_left
        (* 整体逻辑为 对于判断最左边点 假设当前向量为最左边的 *)
        (* 通过迭代和叉乘判断下一个点是否在当前点的左边 *)
        (* d -> droite; g -> gauche *)
          (fun (d, g) p ->
          let ap = diff_2d p a in
          let ad = diff_2d d a and ag = diff_2d g a in
          let relative_ad_ap = vectoriel_2d ad ap in
          (* 找最左边的点 *)
          if relative_ad_ap < 0. then (p, g)
          else
            let relative_ag_ap = vectoriel_2d ag ap in
            (* 找最右边的点 *)
            if relative_ag_ap > 0. then (d, p) else (d, g))
        (hd, hd) l

let pi = acos (-1.)
let infinity = 1. /. 0.

(* 求的是c点到 过a点以v为向量的 直线的向量（方向指向c） *)
(* d 是c点到 过a点以v为向量的 直线的距离 *)
(* x,y 是 d 向量（指向c）的坐标 *)
let projection_point_to_vector c a v =
  if scal_2d v v = 0. then failwith "projection sur une droite sans direction";
  let ac = diff_2d c a in
  let alpha = angle_2d v in
  let d = vectoriel_2d ac v /. norm_2d v in
  (* if angle_2d ac > angle_2d v then
       { x = d *. cos (alpha +. (pi /. 2.)); y = d *. sin (alpha +. (pi /. 2.)) }
     else
       { x = d *. cos (alpha -. (pi /. 2.)); y = d *. sin (alpha -. (pi /. 2.)) } *)
  { x = d *. cos (alpha +. (pi /. 2.)); y = d *. sin (alpha +. (pi /. 2.)) }

(* 求的是c点到 ab 线段 的最小距离 以及 该点 *)
(* 投影点不再 ab 之内时， 取 a 或 b 点 *)
let projection_point_to_segment c a b =
  let ab = diff_2d b a and ac = diff_2d c a and bc = diff_2d c b in
  if scal_2d ab ab = 0. then (norm_2d ac, a)
  else if scal_three_point_2d b a c <= 0. then (norm_2d ac, a)
  else if scal_three_point_2d a b c <= 0. then (norm_2d bc, b)
  else
    let unit_ab = resize_2d ab (norm_2d ab) in
    let distance = abs_float (vectoriel_three_point_2d b a c) /. norm_2d ab
    and delta = abs_float (scal_three_point_2d b a c) /. norm_2d ab in
    (distance, diff_2d a (resize_2d (opp_2d unit_ab) (1. /. delta)))

let projection_point_to_convex a convex =
  let rec projection_each_edge a convex last_node distance project_point =
    match convex with
    | [] -> project_point
    | node :: tl ->
        let new_distance, new_project_point =
          projection_point_to_segment a last_node node
        in
        if new_distance < distance then
          projection_each_edge a tl node new_distance new_project_point
        else projection_each_edge a tl node distance project_point
  in
  match convex with
  | [] -> failwith "convex vide"
  | hd :: tl -> projection_each_edge a convex hd infinity hd

(* pa -> a 直线上任意一点
   na -> a 直线法向向量
   pb nb 同理 *)
exception Droites_confondues
exception No_solution

(* pa -> point a
   na -> normal vector a
   pb -> point b
   nb -> normal vector b *)
let intersection_point_of_two_line pa na pb nb =
  (* 判断两条直线是否平行或垂直 *)
  (* 判断两个法向向量是否方向相同 *)
  let d = vectoriel_2d na nb in
  if abs_float d < Const.epsilon then
    let pr = diff_2d nb na in
    (* 判断两条直线是否共线 *)
    if abs_float (scal_2d pr na) < Const.epsilon then (
      Printf.printf "Droites_confondues pr=%f %f\n" pr.x pr.y;
      Printf.printf "Droites_confondues na=%f %f\n" na.x na.y;
      flush stdout;
      raise Droites_confondues)
    else (
      Printf.printf "No_solution pr=%f %f\n" pr.x pr.y;
      Printf.printf "No_solution na=%f %f\n" na.x na.y;
      flush stdout;
      raise No_solution)
  else
    (* na.x + nb.y = na.x *. pa.x + na.y *. pa.y 为 a 的直线方程 *)
    (* 克莱姆法则 求解 a b 两方程的焦点 *)
    let ca = scal_2d na pa and cb = scal_2d nb pb in
    {
      x = vectoriel_2d (create_t ca na.y) (create_t cb nb.y) /. d;
      y = vectoriel_2d (create_t na.x ca) (create_t nb.x cb) /. d;
    }

exception Vide

(* 使cadre首尾相接 *)
let complete cadre =
  match cadre with
  | [] -> []
  | deb :: tl -> (
      match List.rev tl with
      | [] -> cadre
      | fin :: _ -> if deb = fin then cadre else fin :: cadre)

let cutting_border point normal_vector cadre =
  (* Printf.printf "point normal %f %f %f %f %d \n" point.x point.y normal_vector.x
     normal_vector.y (List.length cadre); *)
  let intersection_border_speed last_node node point normal_vector =
    intersection_point_of_two_line node
      (normal_vecteur_for_two_point_2d last_node node)
      point normal_vector
  in
  let get_flag a b v = scal_2d (diff_2d a b) v >= 0. in
  let rec intersect point normal_vector cadre last_flag last_node result_list =
    match cadre with
    | [] -> (
        match result_list with
        | [] ->
            Printf.printf "\027[32m Geom.Vide \027[0m \n";
            raise Vide
        | _ -> complete (List.rev result_list))
    | node :: tl ->
        if point = node then
          intersect point normal_vector tl last_flag node result_list
        else
          let flag = get_flag node point normal_vector in
          if last_flag then
            if flag then
              intersect point normal_vector tl last_flag node
                (node :: result_list)
            else
              let intersect_point =
                intersection_border_speed last_node node point normal_vector
              in
              intersect point normal_vector tl false node
                (intersect_point :: result_list)
          else if flag then
            let intersect_point =
              intersection_border_speed last_node node point normal_vector
            in
            intersect point normal_vector tl true node
              (node :: intersect_point :: result_list)
          else intersect point normal_vector tl last_flag node result_list
  in
  match cadre with
  | [] -> []
  | hd :: tl ->
      (* Printf.printf "cadre:%d\n" (List.length cadre);

         let nb = ref (List.length cadre) and i = ref 0 in
         while !nb > 0 do
           let temp = List.nth cadre !i in
           Printf.printf "%f %f\n" temp.x temp.y;
           nb := !nb - 1;
           i := !i + 1
         done;
         Printf.printf "\n"; *)
      intersect point normal_vector tl (get_flag hd point normal_vector) hd []
