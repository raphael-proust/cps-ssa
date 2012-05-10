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
  let () = assert (SSA.check_ssa prog) in

  let term = SSA2CPS.prog prog in

  let b = Buffer.create 10 in
  let f = Format.formatter_of_buffer b in
  let () = Format.fprintf f "@[%a@]" CPS_diff.print_m term in
  let () = Format.pp_print_newline f () in
  let () = Format.pp_print_newline f () in
  let () = print_string (Buffer.contents b) in
  let () = Buffer.clear Format.stdbuf in
  ()

let zero =
  ("zero", [SSA.Procs.block [] (SSA.Blocks.zero ~label:SSA.label_main ())])

let cond =
  ("cond", [SSA.Procs.cond ~label:SSA.label_main []
             Prim.(ONone (Vconst 0))
             (SSA.Blocks.const ~label:(Prim.label "true") 1)
             (SSA.Blocks.const ~label:(Prim.label "false") 2)
           ]
  )

let diamond =
  let var v = Prim.(ONone (Vvar v)) in

  let entry_var = Prim.var "arg0" in

  let true_label  = Prim.label "true" in
  let var_true = Prim.fresh_var () in

  let false_label = Prim.label "false" in
  let var_false = Prim.fresh_var () in

  let merge_label = Prim.label "merge" in
  let var_merge = Prim.fresh_var () in

  let merge_block = {SSA.
    b_order = 0;
    b_label = merge_label;
    b_phis =
      [(var_merge, [(true_label, var var_true);
                    (false_label, var var_false);
                   ]
       );
      ];
    b_assigns = [];
    b_jump = SSA.Jreturn (var var_merge);
  }
  in

  let true_block = SSA.Blocks.expr ~label:true_label (var var_true) in
  let true_block = {true_block with
    SSA.b_assigns = [SSA.Aexpr (var_true, Prim.(ONone (Vconst 42)))];
    SSA.b_jump = SSA.Jgoto merge_label;
  }
  in

  let false_block = SSA.Blocks.expr ~label:false_label (var var_false) in
  let false_block = {false_block with
    SSA.b_assigns = [SSA.Aexpr (var_false, Prim.(ONone (Vconst 37)))];
    SSA.b_jump = SSA.Jgoto merge_label;
  }
  in

  let entry_block = SSA.Blocks.cond ~label:SSA.label_main
    (var entry_var) true_label false_label
  in

  let diamond = {SSA.
      p_args = [entry_var];
    p_blocks = [entry_block; merge_block; true_block; false_block];
  }
  in

  ("diamond",[diamond])

let tests = [
  zero;
  cond;
  diamond;
]

let () =
    List.iter
      (fun t ->
        try
          run t
        with
        | e ->
          Printf.eprintf "%s\n" (Printexc.to_string e);
          Printexc.print_backtrace stderr
      )
      tests
