 (* {{{ LICENSE                                                              *
  * vi: set fdm=marker fdl=0:                                                *
  *                                                                          *
  * Copyright (c) 2012 Raphaël Proust <raphlalou@gmail.com>                  *
  * Copyright (c) 2012 INRIA - Raphaël Proust <raphlalou@gmail.com>          *
  * Copyright (c) 2012 ENS - Raphaël Proust <raphlalou@gmail.com>            *
  *                                                                          *
  * Permission to use, copy, modify, and distribute this software for any    *
  * purpose with or without fee is hereby granted, provided that the above   *
  * copyright notice and this permission notice appear in all copies.        *
  *                                                                          *
  * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES *
  * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         *
  * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  *
  * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   *
  * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    *
  * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  *
  * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           *
  * }}}                                                                      *)

let w_buf m =
  let b = Buffer.create 10 in
  let f = Format.formatter_of_buffer b in
  let () = Format.fprintf f "@[%a@]" CPS_diff.print_m m in
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
