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

let run filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let llvm_prog = Llvm_parser.module_ (Llvm_lexer.token) lexbuf in
    ignore llvm_prog
  with
  | e ->
    Printf.eprintf "Uncaught exception while lexing/parsing from %a to %a"
    Util.P.print_pos (Lexing.lexeme_start_p lexbuf)
    Util.P.print_pos (Lexing.lexeme_end_p   lexbuf);
    raise e

let () =
    Array.iter
      (fun t ->
        try
          run t
        with
        | e ->
          Printf.eprintf "%s\n" (Printexc.to_string e);
          Printexc.print_backtrace stderr
      )
      (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
