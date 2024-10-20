open FlyWithVectical

let () =
  let flag_fin = ref 0 in
  let time = ref 0 in
  let step = 10 (*pas de temps affichage *) in
  let acfts = Aircraft.acft_lst in

  Plot.output_routes acfts;
  Plot.output_obstacle;

  while is_stop Const.dim !flag_fin do
    let constraints = Array.init Const.dim (fun i -> []) in
    for i = 0 to Const.dim - 1 do
      if (List.nth acfts i).active then (
        get_constraints_entre_avions i acfts constraints;
        get_constraints_obstacle i acfts constraints)
    done;

    let boites = Array.init Const.dim (fun i -> []) in
    for i = 0 to Const.dim - 1 do
      let targetbox =
        get_available_speed_box (List.nth acfts i).speed constraints.(i)
      in

      let new_speed =
        if List.length targetbox == 1 && List.nth targetbox 0 == Geom.default_t
        then Geom.default_t
        else
          get_available_speed (List.nth acfts i).speed
            (List.nth acfts i).speedopt targetbox
      in
      (List.nth acfts i).speed <- new_speed;
      if new_speed == Geom.default_t then (List.nth acfts i).level <- 1;
      boites.(i) <- targetbox
    done;
    if !time mod step = 0 then (
      Plot.output acfts boites !time;
      flush Plot.outc);
    move_all Const.dim acfts flag_fin;
    Printf.printf "\027[32m time: %d \027[0m \n" !time;
    Printf.printf "\027[32m flag_fin: %d \027[0m \n" !flag_fin;
    incr time;
    if !time > 5000 then exit 1
  done;
  let _ = Unix.select [] [] [] 10. in
  flush Plot.outc
