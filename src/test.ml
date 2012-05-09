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

let () = Printexc.record_backtrace true

let run (id, prog) =
  let () = Printf.printf "\t%s: Checking well-formedness\n" id in
  let () = assert (SSA.check_ssa prog) in

  let () = Printf.printf "\t%s: Translating\n" id in
  let run = CPS.Cvar (Prim.var "run") in
  let term = SSA2CPS.prog prog run in

  let () = Printf.printf "\t%s: Printing\n" id in
  let b = Buffer.create 10 in
  let f = Format.formatter_of_buffer b in
  let () = Format.fprintf f "@[%a@]" CPS_diff.print_m term in
  let () = Format.pp_print_newline f () in
  let () = Format.pp_print_newline f () in
  let () = print_string (Buffer.contents b) in
  let () = Buffer.clear Format.stdbuf in
  ()

let tests = [
  "zero", [SSA.Procs.block [] (SSA.Blocks.zero ~label:SSA.label_main ())];
]

let () =
  try
    List.iter run tests
  with
  | e ->
    Printexc.print_backtrace stderr;
    raise e
