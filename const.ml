let get_sep () =
  (* 打印提示信息 *)
  Printf.printf
    "Entrez le nombre de normes de separations pour créer les orig-dest ";
  (* 刷新输出缓冲区，确保提示信息立即显示 *)
  flush stdout;
  (* 获取用户输入并解析为整数 *)
  try
    let sep = read_float () in
    Printf.printf "le nombre de normes de separations est: %f\n" sep;
    sep
  with Failure _ ->
    Printf.printf
      "L'entrée du nombre de normes de séparation pour créer les orig-dest est \
       invalide\n";
    exit 1

(* let sep = get_sep () *)
let sep = 5.

let get_norme () =
  (* 打印提示信息 *)
  Printf.printf "Entrez la demi norme de separation";
  (* 刷新输出缓冲区，确保提示信息立即显示 *)
  flush stdout;
  (* 获取用户输入并解析为整数 *)
  try
    let norme = read_float () in
    Printf.printf "la demi norme de separation est: %f\n" norme;
    norme
  with Failure _ ->
    Printf.printf "L'entrée de la demi norme de separation est invalide\n";
    exit 1

(* let norme = get_norme () *)
let norme = 2.5
let norme2 = 4. *. norme *. norme (*norme de separation au carré*)

(* nb de drones *)
let get_dim () =
  (* 打印提示信息 *)
  Printf.printf "input the number of drones: ";
  (* 刷新输出缓冲区，确保提示信息立即显示 *)
  flush stdout;
  (* 获取用户输入并解析为整数 *)
  try
    let dim = read_int () in
    Printf.printf "the number of drones is: %d\n" dim;
    dim
  with Failure _ ->
    Printf.printf "invalid input of the number of drones\n";
    exit 1

let dim = get_dim ()
(* let dim = 10 *)

let get_speed () =
  (* 打印提示信息 *)
  Printf.printf "input the speed of drones: ";
  (* 刷新输出缓冲区，确保提示信息立即显示 *)
  flush stdout;
  (* 获取用户输入并解析为整数 *)
  try
    let speed = read_float () in
    Printf.printf "the speed of drones is: %f\n" speed;
    speed
  with Failure _ ->
    Printf.printf "invalid input of the speed of drones\n";
    exit 1

let const_speed = get_speed ()
(* let const_speed = 1. *)

let get_pas () =
  (* 打印提示信息 *)
  Printf.printf "input the time step: ";
  (* 刷新输出缓冲区，确保提示信息立即显示 *)
  flush stdout;
  (* 获取用户输入并解析为整数 *)
  try
    let speed = read_float () in
    Printf.printf "the speed of drones is: %f\n" speed;
    speed
  with Failure _ ->
    Printf.printf "invalid input of the speed of drones\n";
    exit 1

(* let pas = get_pas () *)
let pas = 1.
let epsilon = 0.0