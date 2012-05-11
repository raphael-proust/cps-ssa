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
  let doc = CPS_diff.pp_m term in
  let () = Pprint.Buffer.pretty 1. 100 b doc in
  let () = print_string (Buffer.contents b) in
  let () = print_string "\n\n" in
  ()

let zero =
  ("zero", [SSA.Procs.block [] (SSA.Blocks.zero ~label:SSA.label_main ())])

let cond =
  ("cond", [SSA.Procs.cond ~label:SSA.label_main []
             Prim.(ONone (Vconst 0))
             (SSA.Blocks.const 1)
             (SSA.Blocks.const 2)
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

  let merge_block =
    SSA.Blocks.expr
      ~label:merge_label
      ~phis:[(var_merge, [(true_label, var var_true);
                          (false_label, var var_false);
                         ]
             );
            ]
    (var var_merge)
  in

  let true_block =
    SSA.Blocks.goto
      ~label:true_label
      ~assigns:[SSA.Aexpr (var_true, Prim.(ONone (Vconst 42)))]
      merge_label
  in

  let false_block =
    SSA.Blocks.goto
      ~label:false_label
      ~assigns:[SSA.Aexpr (var_false, Prim.(ONone (Vconst 42)))]
      merge_label
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

let call =
  let ret_label = Prim.fresh_label () in
  let ret =
    let arg = Prim.fresh_var () in
    SSA.(Procs.block
          [arg]
          (Blocks.expr ~label:ret_label Prim.(ONone (Vvar arg)))
    )
  in
  let entry =
    let arg = Prim.var "arg0" in
    let res = Prim.fresh_var () in
    {SSA.
      p_args = [arg];
      p_blocks = [{SSA.
        b_order = 0;
        b_label = SSA.label_main;
        b_phis = [];
        b_assigns = [SSA.Acall (res,
                                ret_label,
                                [Prim.(ONone (Vvar arg))]
                               )
                    ];
        b_jump = SSA.Jreturn Prim.(ONone (Vvar res));
      }]
    }
  in
  ("call",[entry; ret])

let loop =
  ("loop", [SSA.(Procs.block [] (Blocks.tail ~label:label_main label_main []))])


let tests = [
  zero;
  cond;
  diamond;
  call;
  loop;
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
      (List.filter
        (fun (name, _) ->
          Array.fold_left (fun f x -> f || x = name) false Sys.argv
        )
        tests
      )
