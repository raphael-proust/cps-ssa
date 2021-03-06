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

(* Default values *)

let ll_files = ref []

let optimisations = ref ([], [])

let verbose = ref false

(*Parsing *)

let dashdash = ref false

let witness_dashdash () =
  if !dashdash then
    let (nu, olds) = !optimisations in
    optimisations := ([], nu :: olds)
  else
    dashdash := true

let witness_opt o =
  let (nu, olds) = !optimisations in
  optimisations := (o :: nu, olds)

let add_ll_file s =
  if s = "-v" then
    verbose := true
  else if Filename.check_suffix s ".ll" then
    ll_files := s :: !ll_files
  else
    raise (Arg.Bad (s ^ " is not an .ll file"))

let parse_fn = function
  | "--" -> witness_dashdash ()
  | "-verbose" -> verbose := true
  | s ->
    if !dashdash then
      witness_opt s
    else
      add_ll_file s

let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1)

let () = Array.iter parse_fn args


(* Exporting dereferenced values *)

let ll_files = !ll_files
let optimisations = let (nu, olds) = !optimisations in List.rev (nu :: olds)
let verbose = !verbose
