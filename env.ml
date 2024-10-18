open Geom

let obstacle =
  [|
    List.rev
      [
        Geom.create_t 50.000000 (-100.000000);
        Geom.create_t 50.000000 200.000000;
        Geom.create_t 100.000000 200.000000;
        Geom.create_t 100.000000 (-100.000000);
        Geom.create_t 50.000000 (-100.000000);
      ];
    List.rev
      [
        Geom.create_t (-150.000000) (-120.000000);
        Geom.create_t (-150.000000) 10.000000;
        Geom.create_t 10.000000 10.000000;
        Geom.create_t 10.000000 (-120.000000);
        Geom.create_t (-150.000000) (-120.000000);
      ];
    List.rev
      [
        Geom.create_t (-100.000000) 50.000000;
        Geom.create_t (-100.000000) 100.000000;
        Geom.create_t (-50.000000) 100.000000;
        Geom.create_t (-50.000000) 50.000000;
        Geom.create_t (-100.000000) 50.000000;
      ];
  |]
