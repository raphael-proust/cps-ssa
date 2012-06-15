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

let run ll_file =

  let () = Printf.printf "Translating %s\n" ll_file in

  let base_file = Filename.chop_suffix ll_file ".ll" in

  (* Phase 1: parse LLVM *)
  let in_ll = open_in ll_file in
  let lexbuf  = Lexing.from_channel in_ll in
  let llvm_prog =
    try
        Llvm_parser.module_ (Llvm_lexer.token) lexbuf
    with
    | e ->
      Printf.eprintf "Uncaught exception while lexing/parsing %s: %a -> %a\n"
        base_file
        Util.P.print_pos (Lexing.lexeme_start_p lexbuf)
        Util.P.print_pos (Lexing.lexeme_end_p   lexbuf);
      raise e
  in
  let () = close_in in_ll in

  (* Phase 2: transform to SSA *)
  let ssa_prog =
    try
      LLVM2SSA.prog llvm_prog
    with
    | e ->
      Printf.eprintf "Uncaught exception while translating %s from LLVM to SSA\n"
        base_file;
        raise e
  in
  let ssa_doc = SSA_pp.pp_prog ssa_prog in
  let ssa_file = base_file ^ ".ssa" in
  let out_ssa = open_out ssa_file in
  let () = Pprint.Channel.pretty 1. 20 out_ssa ssa_doc in
  let () = close_out out_ssa in
  let () = SSA.check_ssa ssa_prog in

  (* Phase 3: transform to CPS *)
  let cps_m =
    try
      SSA2CPS.prog ssa_prog
    with
    | e ->
      Printf.eprintf "Uncaught exception while translating %s SSA to CPS\n"
        base_file;
      raise e
  in
  let cps_doc = CPS_pp.pp_m cps_m in
  let cps_file = base_file ^ ".cps" in
  let out_cps = open_out cps_file in
  let () = Pprint.Channel.pretty 0. 100 out_cps cps_doc in
  let () = close_out out_cps in
  ()

let generate_optimised ll_file optimisations =
  let base_file = Filename.chop_suffix ll_file ".ll" in
  let optimised =
    List.fold_left
      (fun accu optim ->
        let optimised_name = Printf.sprintf "%s.%s.ll" base_file optim in
        let command =
          (Printf.sprintf "opt -S -o %s.%s.ll -%s %s"
            base_file optim optim ll_file
          )
        in
        Printf.printf "Executing: %s\n" command;
        let rc = Sys.command command in
        flush_all ();
        let accu =
          if rc = 0 then
            optimised_name :: accu
          else begin
            Printf.eprintf "Error occured on optimisation %s on file %s\n"
              optim ll_file;
            accu
          end
        in
        flush_all ();
        accu
      )
      []
      optimisations
  in
  ll_file :: optimised


let () =
    List.iter
      (fun t ->
        try
          let lls = generate_optimised t Options.optimisations in
          List.iter run lls
        with
        | e ->
          Printf.eprintf "%s\n" (Printexc.to_string e);
          Printexc.print_backtrace stderr
      )
      Options.ll_files
