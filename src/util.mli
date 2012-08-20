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

module E :
  sig
    type ('a, 'b) either = Left of 'a | Right of 'b
    val left : 'a -> ('a, 'b) either
    val right : 'a -> ('b, 'a) either
  end
module I :
  sig
    val fold_inc : ('a -> int -> 'a) -> 'a -> int -> 'a
  end
module L :
  sig
    val concat_map : ('a -> 'b list) -> 'a list -> 'b list
    val exists_one : ('a -> bool) -> 'a list -> bool
    val unique : ('a -> 'b option) -> 'a list -> bool
    val pick_one_such_as : ('a -> bool) -> 'a list -> 'a * 'a list
    val map_option : ('a -> 'b option) -> 'a list -> 'b list
    val inter : 'a list -> 'a list -> 'a list
    val minus : 'a list -> 'a list -> 'a list
    val includes : 'a list -> 'a list -> bool
    val disjoint : 'a list -> 'a list -> bool
    val cat_uniq : 'a list -> 'a list -> 'a list
    val take : 'a list -> int -> 'a list
    val n : (int -> 'a) -> int -> 'a list
    val nconst : 'a -> int -> 'a list
    val classes : ('a * 'b) list -> ('a * 'b list) list
  end
module O :
  sig
    val opt : 'a -> 'a option
    val none : 'a option
    val unopt_soft : (unit -> 'a) -> 'a option -> 'a
    val unopt : 'a -> 'a option -> 'a
    val unopt_hard : 'a option -> 'a
  end
module P :
  sig
    val pos_char : Lexing.position -> int
    val pos_line : Lexing.position -> int
    val print_pos : out_channel -> Lexing.position -> unit
    exception Lex_error_unterminated_string of Lexing.position
  end
module PP :
  sig
    val with_paren : Pprint.document -> Pprint.document
    val with_paren_br : Pprint.document -> Pprint.document
    val comma_space : Pprint.document
    val list :
      ?empty:Pprint.document ->
      ?sep:Pprint.document ->
      ('a -> Pprint.document) -> 'a list -> Pprint.document
    val either : ('a -> 'b) -> ('c -> 'b) -> ('a, 'c) E.either -> 'b
    val level : Pprint.document -> Pprint.document
    val unit : Pprint.document
    val op :
      ('a -> Pprint.document) ->
      'a -> Pprint.document -> 'a -> Pprint.document
    val fn :
      ('a -> Pprint.document) ->
      Pprint.document -> 'a list -> Pprint.document
  end
