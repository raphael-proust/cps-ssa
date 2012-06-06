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

module L = struct

  let concat_map f l = List.concat (List.map f l)

  (* [exists_one p l] tests wheter the predicate [p] is true for exactly one
   * element of the list [l]. *)

  let exists_one predicate l =
    let rec aux flag = function
      | [] -> flag
      | h::t ->
        let f = predicate h in
        (not (flag && f)) || (aux (flag || f) t)
    in
    aux false l

  let unique extract l =
    let hshtbl = Hashtbl.create 32 in
    let rec aux = function
      | [] -> true
      | h::t ->
        let x = extract h in
        match x with
        | Some x -> begin
          let h = Hashtbl.hash x in
          try
            if Hashtbl.find hshtbl h = x then
              false
            else
              raise Not_found (*hackish way to DRY*)
          with
            | Not_found ->
              Hashtbl.add hshtbl h x;
              aux t
          end
        | None -> aux t
    in
    aux l

  let pick_one_such_as f l =
    let rec aux accu = function
      | [] -> raise Not_found
      | h::t ->
        if f h then
          (h, List.rev_append accu t)
        else
          aux (h::accu) t
    in
    aux [] l


end

module O = struct

  let opt x = Some x

  let none = None

  let unopt = function
    | None -> assert false
    | Some v -> v

end

module P = struct

  let pos_char pos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
  let pos_line pos = pos.Lexing.pos_lnum

  let print_pos ch pos =
    Printf.fprintf ch "line %d, character %d" (pos_line pos) (pos_char pos)

end


