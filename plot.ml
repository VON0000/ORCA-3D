open Aircraft
open Geom

let outc = Unix.open_process_out "/usr/bin/gnuplot"
let radius = 300. (*taille de la fenetre*)
let dscreen = 50 + truncate radius

(*taille de la fenetre étendue*)
(* truncate 取整*)
let delai = 0.5 (*delai pour ralentir l'affichage*)

let output_routes acfts =
  let fichroutes = open_out "./results/routes" in
  for i = 0 to Const.dim - 1 do
    List.iter
      (fun p -> Printf.fprintf fichroutes "%f %f\n" p.x p.y)
      (List.nth acfts i).route;
    Printf.fprintf fichroutes "\n"
  done;
  close_out fichroutes;
  ()

let output_obstacle =
  let fichobstacle = open_out "./results/obstacle" in
  for i = 0 to Array.length Env.obstacle - 1 do
    List.iter
      (fun p -> Printf.fprintf fichobstacle "%f %f\n" p.x p.y)
      Env.obstacle.(i);
    Printf.fprintf fichobstacle "\n"
  done;
  close_out fichobstacle;
  ()

(* 利用gnuplot绘图 *)
let plot_to_screen time =
  Printf.fprintf outc "set terminal x11\n";
  (* 使用 x11 作为终端 *)
  Printf.fprintf outc "set xrange [-%d:%d]\n" dscreen dscreen;
  Printf.fprintf outc "set yrange [-%d:%d]\n" dscreen dscreen;

  (* 使用 replot 以确保图形能更新 *)
  Printf.fprintf outc
    "plot './results/obstacle' not w l lw 3, './results/points' not pt 7 ps \
     2, './results/speeds' w l, './results/newspeeds' w l, './results/routes' \
     lc 4 w l, './results/boites' w l, './results/memory' not w l lc 3\n";

  flush outc;

  (* 确保命令发送 *)

  (* 添加短暂延时，确保图形有时间渲染 *)
  Unix.sleep 1;
  ()

let output acfts boites time =
  let fichobstacle = open_out "./results/obstacle" in
  for i = 0 to Array.length Env.obstacle - 1 do
    List.iter
      (fun p -> Printf.fprintf fichobstacle "%f %f\n" p.x p.y)
      Env.obstacle.(i);
    Printf.fprintf fichobstacle "\n"
  done;
  close_out fichobstacle;

  let coeff = 50. (*zoom d'affichage des rayons des vitesse*) in
  let fichp = open_out "./results/points" in
  let fichb = open_out "./results/boites" in
  let fichs = open_out "./results/speeds" in
  let fichns = open_out "./results/newspeeds" in
  for i = 0 to Const.dim - 1 do
    let local_acft = List.nth acfts i in
    if local_acft.active then (
      Printf.fprintf fichp "%f %f\n" local_acft.position.x local_acft.position.y;
      flush fichp;
      Printf.fprintf fichs "%f %f\n%f %f\n\n" local_acft.position.x
        local_acft.position.y
        (local_acft.position.x +. (coeff *. local_acft.speedopt.x))
        (local_acft.position.y +. (coeff *. local_acft.speedopt.y));
      flush fichs;
      Printf.fprintf fichns "%f %f\n%f %f\n\n" local_acft.position.x
        local_acft.position.y
        (local_acft.position.x +. (coeff *. local_acft.speed.x))
        (local_acft.position.y +. (coeff *. local_acft.speed.y));
      flush fichns;
      List.iter
        (fun { x; y } ->
          Printf.fprintf fichb "%f %f\n"
            ((x *. coeff) +. local_acft.position.x)
            ((y *. coeff) +. local_acft.position.y))
        boites.(i);
      Printf.fprintf fichb "\n";
      flush fichb)
  done;
  close_out fichp;
  close_out fichb;
  close_out fichs;
  close_out fichns;
  plot_to_screen time;
  ()

(* let output_memory acfts =
   let fichmem = open_out "./results/memory" in
   for i = 0 to Const.dim - 1 do
     let local_acft = List.nth acfts i in
     if local_acft.active then (
       Printf.fprintf fichmem "%f %f\n" local_acft.position.x
         local_acft.position.y;
       flush fichmem)
   done;
   close_out fichmem;
   () *)