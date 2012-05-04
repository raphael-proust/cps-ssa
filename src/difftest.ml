module Cd = CPS_diff ;;

let w_buf m =
  let b = Buffer.create 10 in
  let f = Format.formatter_of_buffer b in
  let () = Format.fprintf f "@[%a@]" Cd.print_m m in
  let () = Format.pp_print_newline f () in
  let () = Format.pp_print_newline f () in
  let () = print_string (Buffer.contents b) in
  let () = Buffer.clear Format.stdbuf in
  ()
;;

let () =
  let open CPS in
  let open Prim in
  List.iter w_buf [
    Mapp (var "foo", [], Cvar (var "bar")) ;

    Mapp (
      var "foo",
      [ONone (Vvar (var "toto"));
      OMult (Vconst 1, Vconst 2);
      ],
      Cvar (var "bar")
    ) ;

    Mcont (var "foo", []) ;

    Mcont (var "foo", [OPlus (Vconst 0, Vconst 1)]) ;

    Mcond ((OMinus (Vvar (var "coucou"), Vconst 42)),
      (var "blah", []),
      (var "asdf", (List.map (fun i -> ONone (Vconst i)) [33; 42; 57])
      )
    ) ;

    (let qwer = var "qwer" in
    Mlet (qwer, OMax (Vconst 33, Vconst 1),
          Mcont (var "return", [ONone (Vvar (qwer))])
    ));

    (let f = var "f" in
    let g = var "g" in
    let x = var "x" in
    Mrec ([(f, Lproc ([x], g, Mcont (g, [OPlus (Vconst 1, Vvar x)])));
           (g, Lproc ([x], f, Mcont (f, [OPlus (Vconst 1, Vvar x)])));
          ],
          Mapp (g, [ONone (Vconst 1)], Cvar (var "crash"))
    ));

    ]
;;
